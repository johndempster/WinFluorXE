unit WatershedParticleLocator;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math;

type
  // Particle information record
  TParticleInfo = record
    ID: Integer;
    PixelLabel: Integer;
    PixelArea: Integer;
    Centroid_X: Double;
    Centroid_Y: Double;
    BoundingBox_Left: Integer;
    BoundingBox_Top: Integer;
    BoundingBox_Right: Integer;
    BoundingBox_Bottom: Integer;
    Perimeter: Integer;
    Circularity: Double; // 4π * Area / Perimeter²
    MeanIntensity: Double;
    Pixels: TArray<TPair<Integer, Integer>>;
  end;

  TParticleArray = TArray<TParticleInfo>;

  // Watershed-based particle locator
  TWatershedParticleLocator = class
  private
    FImage: TArray<TArray<Byte>>;
    FWidth: Integer;
    FHeight: Integer;


    FGradient: TArray<TArray<Word>>;

    FParticles: TParticleArray;
    FThreshold: Word ;

    procedure ComputeGradient;

    procedure ComputeDistanceTransform1;
    procedure FindLocalMinima;
    procedure LabelLocalMinima;
    procedure WatershedPropagation;
    procedure ExtractParticles;
    function GetNeighbors(X, Y: Integer): TArray<TPair<Integer, Integer>>;
    function GetNeighbors4(X, Y: Integer): TArray<TPair<Integer, Integer>>;
    function IsValidPixel(X, Y: Integer): Boolean;
    procedure CalculateParticleProperties(var Particle: TParticleInfo; Pixels: TArray<TPair<Integer, Integer>>);

  public
  FPixelLabels: TArray<TArray<Integer>>;
  FDistance: TArray<TArray<Single>>;
  FMarkers: TArray<TArray<Integer>>;
    constructor Create(Image: TArray<TArray<Byte>>; Width, Height: Integer);
    destructor Destroy; override;
    procedure ComputeDistanceTransform;
    procedure WatershedFlood ;
    procedure LocateParticles(Threshold: Word = 50);
    function GetParticles: TParticleArray;
    function GetParticleCount: Integer;
    function GetLabels: TArray<TArray<Integer>>;
    function GetGradient: TArray<TArray<Word>>;
  end;

implementation

{ TWatershedParticleLocator }

constructor TWatershedParticleLocator.Create(Image: TArray<TArray<Byte>>; Width, Height: Integer);
var
  I, J: Integer;
begin
  inherited Create;
  FImage := Image;
  FWidth := Width;
  FHeight := Height;
  FThreshold := 50;

  // Initialize arrays
  SetLength(FPixelLabels, FHeight, FWidth);
  SetLength(FDistance, FHeight, FWidth);
  SetLength(FGradient, FHeight, FWidth);
  SetLength(FMarkers, FHeight, FWidth);

  for I := 0 to FHeight - 1 do
  begin
    for J := 0 to FWidth - 1 do
    begin
      FPixelLabels[I, J] := 0;
      FDistance[I, J] := 0;
      FGradient[I, J] := 0;
      FMarkers[I, J] := 0;
    end;
  end;
end;

destructor TWatershedParticleLocator.Destroy;
begin
  inherited;
end;

function TWatershedParticleLocator.IsValidPixel(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight);
end;

function TWatershedParticleLocator.GetNeighbors(X, Y: Integer): TArray<TPair<Integer, Integer>>;
var
  List: TList<TPair<Integer, Integer>>;
  DX, DY: Integer;
  NewX, NewY: Integer;
begin
  List := TList<TPair<Integer, Integer>>.Create;
  try
    // 8-connectivity neighbors
    for DY := -1 to 1 do
    begin
      for DX := -1 to 1 do
      begin
        if (DX <> 0) or (DY <> 0) then
        begin
          NewX := X + DX;
          NewY := Y + DY;
          if IsValidPixel(NewX, NewY) then
            List.Add(TPair<Integer, Integer>.Create(NewX, NewY));
        end;
      end;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TWatershedParticleLocator.GetNeighbors4(X, Y: Integer): TArray<TPair<Integer, Integer>>;
var
  List: TList<TPair<Integer, Integer>>;
  NewX, NewY: Integer;
begin
  List := TList<TPair<Integer, Integer>>.Create;
  try
    // 4-connectivity neighbors (up, down, left, right)
    NewX := X;
    NewY := Y - 1;
    if IsValidPixel(NewX, NewY) then
      List.Add(TPair<Integer, Integer>.Create(NewX, NewY));

    NewX := X;
    NewY := Y + 1;
    if IsValidPixel(NewX, NewY) then
      List.Add(TPair<Integer, Integer>.Create(NewX, NewY));

    NewX := X - 1;
    NewY := Y;
    if IsValidPixel(NewX, NewY) then
      List.Add(TPair<Integer, Integer>.Create(NewX, NewY));

    NewX := X + 1;
    NewY := Y;
    if IsValidPixel(NewX, NewY) then
      List.Add(TPair<Integer, Integer>.Create(NewX, NewY));

    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

procedure TWatershedParticleLocator.ComputeGradient;
var
  X, Y: Integer;
  Gx, Gy: Integer;
  Value: Integer;
begin
  // Compute gradient magnitude using Sobel operator
  for Y := 1 to FHeight - 2 do
  begin
    for X := 1 to FWidth - 2 do
    begin
      if FImage[Y, X] > FThreshold then
      begin
        // Sobel X gradient
        Gx := (Integer(FImage[Y - 1, X - 1]) - Integer(FImage[Y - 1, X + 1])) +
              2 * (Integer(FImage[Y, X - 1]) - Integer(FImage[Y, X + 1])) +
              (Integer(FImage[Y + 1, X - 1]) - Integer(FImage[Y + 1, X + 1]));

        // Sobel Y gradient
        Gy := (Integer(FImage[Y - 1, X - 1]) - Integer(FImage[Y + 1, X - 1])) +
              2 * (Integer(FImage[Y - 1, X]) - Integer(FImage[Y + 1, X])) +
              (Integer(FImage[Y - 1, X + 1]) - Integer(FImage[Y + 1, X + 1]));

        // Gradient magnitude
        Value := Trunc(Sqrt(Gx * Gx + Gy * Gy));
        if Value > High(Word) then Value := High(Word) ;
        FGradient[Y, X] := Word(Value);
      end;
    end;
  end;
end;

procedure TWatershedParticleLocator.ComputeDistanceTransform;
var
  X, Y: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I : Integer;
  Dmax{,DMin} : SIngle ;
  Neighbor: TPair<Integer, Integer>;
begin

  // Initialize distance transform
  for Y := 0 to FHeight - 1 do
      begin
      for X := 0 to FWidth - 1 do
         begin
         if FImage[Y, X] > 0 then FDistance[Y, X] := 0
                             else FDistance[Y, X] := 10000 ; // Large number
         end;
      end;

    // Forward pass
    for Y := 0 to FHeight - 1 do
        begin
        for X := 0 to FWidth - 1 do
            begin
            if FImage[Y, X] > 0 then
               begin
               Neighbors := GetNeighbors(X, Y);
               DMax := 0 ;
               for i := 0 to High(Neighbors) do
                   begin
                   Neighbor := Neighbors[i];
                   if (FDistance[Neighbor.Value, Neighbor.Key] > DMax) then DMax := FDistance[Neighbor.Value, Neighbor.Key] ;
                   end ;
               FDistance[Y, X] := DMax - 1 ;
               end;
            end;
        end;

    // Backward pass
    for Y := FHeight - 1 downto 0 do
        begin
        for X := FWidth - 1 downto 0 do
            begin
            if FImage[Y, X] > 0 then
               begin
               Neighbors := GetNeighbors(X, Y);
               DMax := 0 ;
               for i := 0 to High(Neighbors) do
                   begin
                   Neighbor := Neighbors[i];
                   if FDistance[Neighbor.Value, Neighbor.Key] > DMax then DMax := FDistance[Neighbor.Value, Neighbor.Key] ;
                   end ;
               FDistance[Y, X] := DMax - 1 ;
               end;
            end;
        end;

end;

procedure TWatershedParticleLocator.ComputeDistanceTransform1;
var
  X, Y: Integer;
  Changed: Boolean;
  MaxIterations: Integer;
  Iteration: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
  NewDist: Single;
begin

  // Initialize distance transform
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      if FImage[Y, X] > FThreshold then
        FDistance[Y, X] := 0
      else
        FDistance[Y, X] := 10000; // Large number for background
    end;
  end;

  // Iterative distance computation (forward and backward passes)
  MaxIterations := Max(FWidth, FHeight);
  for Iteration := 0 to MaxIterations - 1 do
  begin
    Changed := False;

    // Forward pass
    for Y := 0 to FHeight - 1 do
    begin
      for X := 0 to FWidth - 1 do
      begin
        if FImage[Y, X] > 0 then
        begin
          Neighbors := GetNeighbors(X, Y);
          for I := 0 to High(Neighbors) do
          begin
            Neighbor := Neighbors[I];
            NewDist := FDistance[Y, X] + 1;
            if NewDist < FDistance[Neighbor.Value, Neighbor.Key] then
            begin
              FDistance[Neighbor.Value, Neighbor.Key] := NewDist;
              Changed := True;
            end;
          end;
        end;
      end;
    end;

    // Backward pass
    for Y := FHeight - 1 downto 0 do
    begin
      for X := FWidth - 1 downto 0 do
      begin
        if FImage[Y, X] > 0  then
        begin
          Neighbors := GetNeighbors(X, Y);
          for I := 0 to High(Neighbors) do
          begin
            Neighbor := Neighbors[I];
            NewDist := FDistance[Y, X] + 1;
            if NewDist < FDistance[Neighbor.Value, Neighbor.Key] then
            begin
              FDistance[Neighbor.Value, Neighbor.Key] := NewDist;
              Changed := True;
            end;
          end;
        end;
      end;
    end;

    if not Changed then
      Break;
  end;
end;

procedure TWatershedParticleLocator.FindLocalMinima;
var
  X, Y: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
  IsMinimum: Boolean;
begin
  // Find local minima in the distance transform (within particles)
  for Y := 1 to FHeight - 2 do
      begin
      for X := 1 to FWidth - 2 do
          begin
          if FImage[Y, X] > 0 then
             begin
             IsMinimum := True;
             Neighbors := GetNeighbors(X, Y);

             for I := 0 to High(Neighbors) do
                 begin
                 Neighbor := Neighbors[I];
                 if (FImage[Neighbor.Value, Neighbor.Key] > 0 ) and
                    (FDistance[Neighbor.Value, Neighbor.Key] < FDistance[Y, X]) then
                    begin
                    IsMinimum := False;
                    Break;
                    end;

                 end;

            if IsMinimum and (FDistance[Y, X] > 0) then
               begin
               FMarkers[Y, X] := 1; // Mark as local minimum
               end;
             end;
          end;
      end;
end ;


procedure TWatershedParticleLocator.WatershedFlood ;
var
    X,Y : Integer ;
  DMax: single ;
begin

  // Find largest peak in distance transform

  DMax := 0 ;
  for Y := 0 to FHeight - 1 do
      begin
      for X := 0 to FWidth - 1 do
         begin
         if (FImage[Y, X] > 0) and (FDistance[Y,X] > DMax) then DMax := FDistance[Y,X] ;
         end;
      end;

 { for Dist := DMax DownTo do
      begin
      for Y := 1 to FHeight - 2 do
        begin
        for X := 1 to FWidth - 2 do
            begin
            if (FDistance[Y,X] = Dist then
               begin
               Neighbors := GetNeighbors(X, Y);
               for I := 0 to High(Neighbors) do
                   begin
                   Neighbor := Neighbors[I];
                   if (FImage[Neighbor.Value, Neighbor.Key] > 0 ) and
             (FDistance[Neighbor.Value, Neighbor.Key] > FDistance[Y, X]) then

      if FDistance[ then}

  end;



procedure TWatershedParticleLocator.LabelLocalMinima;
var
  X, Y: Integer;
  CurrentLabel: Integer;
  Queue: TQueue<TPair<Integer, Integer>>;
  Current: TPair<Integer, Integer>;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
begin
  Queue := TQueue<TPair<Integer, Integer>>.Create;
  try
    CurrentLabel := 1;

    // Label each connected component containing a local minimum
    for Y := 0 to FHeight - 1 do
    begin
      for X := 0 to FWidth - 1 do
      begin
        if (FMarkers[Y, X] = 1) and (FPixelLabels[Y, X] = 0) then
        begin
          // BFS from this local minimum
          Queue.Enqueue(TPair<Integer, Integer>.Create(X, Y));
          FPixelLabels[Y, X] := CurrentLabel;

          while Queue.Count > 0 do
          begin
            Current := Queue.Dequeue;
            Neighbors := GetNeighbors4(Current.Key, Current.Value);

            for I := 0 to High(Neighbors) do
            begin
              Neighbor := Neighbors[I];
              if (FPixelLabels[Neighbor.Value, Neighbor.Key] = 0) and
                 (FImage[Neighbor.Value, Neighbor.Key] > 0 ) then
              begin
                FPixelLabels[Neighbor.Value, Neighbor.Key] := CurrentLabel;
                Queue.Enqueue(Neighbor);
              end;
            end;
          end;

          Inc(CurrentLabel);
        end;
      end;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TWatershedParticleLocator.WatershedPropagation;
var
  Queue: TQueue<TPair<Single, TPair<Integer, Integer>>>;
  Current: TPair<Single, TPair<Integer, Integer>>;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
  NeighborLabel: Integer;
  Y, X: Integer;
begin
  Queue := TQueue<TPair<Single, TPair<Integer, Integer>>>.Create;
  try
    // Initialize queue with all labeled pixels, sorted by distance
    for Y := 0 to FHeight - 1 do
    begin
      for X := 0 to FWidth - 1 do
      begin
        if FPixelLabels[Y, X] > 0 then
          Queue.Enqueue(TPair<Single, TPair<Integer, Integer>>.Create(
            FDistance[Y, X], TPair<Integer, Integer>.Create(X, Y)));
      end;
    end;

    // Watershed propagation - expand regions based on distance
    while Queue.Count > 0 do
    begin
      Current := Queue.Dequeue;
      X := Current.Value.Key;
      Y := Current.Value.Value;

      if FImage[Y, X] > 0 then
      begin
        Neighbors := GetNeighbors(X, Y);

        for I := 0 to High(Neighbors) do
        begin
          Neighbor := Neighbors[I];

          if (FPixelLabels[Neighbor.Value, Neighbor.Key] = 0) and
             (FImage[Neighbor.Value, Neighbor.Key] > 0 ) then
          begin
            NeighborLabel := FPixelLabels[Y, X];
            FPixelLabels[Neighbor.Value, Neighbor.Key] := NeighborLabel;
            Queue.Enqueue(TPair<Single, TPair<Integer, Integer>>.Create(
              FDistance[Neighbor.Value, Neighbor.Key], Neighbor));
          end;
        end;
      end;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TWatershedParticleLocator.CalculateParticleProperties(
  var Particle: TParticleInfo; Pixels: TArray<TPair<Integer, Integer>>);
var
  I: Integer;
  Pixel: TPair<Integer, Integer>;
  MinX, MaxX, MinY, MaxY: Integer;
  SumX, SumY, SumIntensity: Int64;
  Perimeter: Integer;
  Neighbor: TPair<Integer, Integer>;
  Neighbors: TArray<TPair<Integer, Integer>>;
  J: Integer;
begin
  if Length(Pixels) = 0 then
    Exit;

  Particle.PixelArea := Length(Pixels);
  Particle.Pixels := Pixels;

  // Initialize bounding box
  MinX := Pixels[0].Key;
  MaxX := Pixels[0].Key;
  MinY := Pixels[0].Value;
  MaxY := Pixels[0].Value;
  SumX := 0;
  SumY := 0;
  SumIntensity := 0;
  Perimeter := 0;

  // Calculate properties
  for I := 0 to High(Pixels) do
  begin
    Pixel := Pixels[I];

    if Pixel.Key < MinX then MinX := Pixel.Key;
    if Pixel.Key > MaxX then MaxX := Pixel.Key;
    if Pixel.Value < MinY then MinY := Pixel.Value;
    if Pixel.Value > MaxY then MaxY := Pixel.Value;

    SumX := SumX + Pixel.Key;
    SumY := SumY + Pixel.Value;
    SumIntensity := SumIntensity + FImage[Pixel.Value, Pixel.Key];

    // Count perimeter pixels (pixels adjacent to background or different region)
    Neighbors := GetNeighbors(Pixel.Key, Pixel.Value);
    for J := 0 to High(Neighbors) do
    begin
      Neighbor := Neighbors[J];
      if (FImage[Neighbor.Value, Neighbor.Key] <= FThreshold) or
         (FPixelLabels[Neighbor.Value, Neighbor.Key] <> Particle.PixelLabel) then
      begin
        Inc(Perimeter);
        Break; // Count this pixel only once
      end;
    end;
  end;

  Particle.BoundingBox_Left := MinX;
  Particle.BoundingBox_Top := MinY;
  Particle.BoundingBox_Right := MaxX;
  Particle.BoundingBox_Bottom := MaxY;

  Particle.Centroid_X := SumX / Particle.PixelArea;
  Particle.Centroid_Y := SumY / Particle.PixelArea;

  Particle.MeanIntensity := SumIntensity / Particle.PixelArea;
  Particle.Perimeter := Perimeter;

  // Calculate circularity: 4π * Area / Perimeter²
  if Perimeter > 0 then
    Particle.Circularity := (4 * PI * Particle.PixelArea) / Sqr(Perimeter)
  else
    Particle.Circularity := 0;
end;

procedure TWatershedParticleLocator.ExtractParticles;
var
  PixelsByLabel: TDictionary<Integer, TList<TPair<Integer, Integer>>>;
  X, Y: Integer;
  PixelLabel: Integer;
  ParticleID: Integer;
  Particle: TParticleInfo;
begin
  PixelsByLabel := TDictionary<Integer, TList<TPair<Integer, Integer>>>.Create;
  try
    // Group pixels by their label
    for Y := 0 to FHeight - 1 do
    begin
      for X := 0 to FWidth - 1 do
      begin
        PixelLabel := FPixelLabels[Y, X];
        if PixelLabel > 0 then
        begin
          if not PixelsByLabel.ContainsKey(PixelLabel) then
            PixelsByLabel.Add(PixelLabel, TList<TPair<Integer, Integer>>.Create);

          PixelsByLabel[PixelLabel].Add(TPair<Integer, Integer>.Create(X, Y));
        end;
      end;
    end;

    // Create particle records
    SetLength(FParticles, PixelsByLabel.Count);
    ParticleID := 0;

    for PixelLabel in PixelsByLabel.Keys do
    begin
      Particle.ID := ParticleID;
      Particle.PixelLabel := PixelLabel;

      CalculateParticleProperties(Particle, PixelsByLabel[PixelLabel].ToArray);

      FParticles[ParticleID] := Particle;
      Inc(ParticleID);
    end;
  finally
    // Cleanup
    for var LabelKey in PixelsByLabel.Keys do
      PixelsByLabel[LabelKey].Free;
    PixelsByLabel.Free;
  end;
end;

procedure TWatershedParticleLocator.LocateParticles(Threshold: Word);
begin

  FThreshold := 0;//Threshold;

  // Step 1: Compute gradient magnitude
  //ComputeGradient;

  // Step 2: Compute distance transform from background
  ComputeDistanceTransform;

  // Step 3: Find local minima as markers for each particle
  FindLocalMinima;

  // Step 4: Label each local minimum region
  LabelLocalMinima;

  // Step 5: Watershed propagation to separate particles
  WatershedPropagation;

  // Step 6: Extract and calculate particle properties
  ExtractParticles;
end;

function TWatershedParticleLocator.GetParticles: TParticleArray;
begin
  Result := FParticles;
end;

function TWatershedParticleLocator.GetParticleCount: Integer;
begin
  Result := Length(FParticles);
end;

function TWatershedParticleLocator.GetLabels: TArray<TArray<Integer>>;
begin
  Result := FPixelLabels;
end;

function TWatershedParticleLocator.GetGradient: TArray<TArray<Word>>;
begin
  Result := FGradient;
end;

end.
