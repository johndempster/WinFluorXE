unit WatershedSegmentation;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math;

type
  // Watershed marker states
  TWatershedState = (
    wsUnmarked = 0,      // Unmarked pixel
    wsLabeledWShed = -1, // Watershed line
    wsLabeledRegion = 1  // Labeled region
  );

  // Record to store watershed result
  TSegmentLabel = record
    Pixel_X: Integer;
    Pixel_Y: Integer;
    iLabel: Integer;
    IsWatershed: Boolean;
  end;

  // Distance and label information
  TDistanceLabel = record
    Distance: Single;
    iLabel: Integer;
  end;

  // Watershed segmentation class
  TWatershedSegmentation = class
  private
    FImage: TArray<TArray<Byte>>;
    FWidth: Integer;
    FHeight: Integer;
    FLabels: TArray<TArray<Integer>>;

    FGradient: TArray<TArray<Byte>>;
    FMarkers: TArray<TArray<Integer>>;

    procedure ComputeGradient;
    procedure ComputeDistanceTransform;
    procedure FindLocalMinima;
    procedure LabelLocalMinima;
    procedure WatershedFromMarkers;
    function GetNeighbors(X, Y: Integer): TArray<TPair<Integer, Integer>>;
    function IsValidPixel(X, Y: Integer): Boolean;
    procedure EuclideanDistance(X, Y: Integer; var Dist: Single);
    
  public
    FDistance: TArray<TArray<Single>>;

    constructor Create(Image: TArray<TArray<Byte>>; Width, Height: Integer);
    destructor Destroy; override;
    
    procedure SegmentImage;
    function GetLabels: TArray<TArray<Integer>>;
    function GetSegmentCount: Integer;
    function GetGradient: TArray<TArray<Byte>>;
    procedure ComputeDistanceTransform1;
  end;

implementation

{ TWatershedSegmentation }

constructor TWatershedSegmentation.Create(Image: TArray<TArray<Byte>>; Width, Height: Integer);
var
  I, J: Integer;
begin
  inherited Create;
  FImage := Image;
  FWidth := Width;
  FHeight := Height;
  
  // Initialize label array
  SetLength(FLabels, FHeight, FWidth);
  SetLength(FDistance, FHeight, FWidth);
  SetLength(FGradient, FHeight, FWidth);
  SetLength(FMarkers, FHeight, FWidth);
  
  for I := 0 to FHeight - 1 do
  begin
    for J := 0 to FWidth - 1 do
    begin
      FLabels[I, J] := 0;
      FDistance[I, J] := 0;
      FGradient[I, J] := 0;
      FMarkers[I, J] := 0;
    end;
  end;
end;

destructor TWatershedSegmentation.Destroy;
begin
  inherited;
end;

function TWatershedSegmentation.IsValidPixel(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight);
end;

function TWatershedSegmentation.GetNeighbors(X, Y: Integer): TArray<TPair<Integer, Integer>>;
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

procedure TWatershedSegmentation.ComputeGradient;
var
  X, Y: Integer;
  Gx, Gy: Integer;
  Value: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
begin
  // Compute gradient magnitude using Sobel operator
  for Y := 1 to FHeight - 2 do
  begin
    for X := 1 to FWidth - 2 do
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
      if Value > 255 then Value := 255;
      FGradient[Y, X] := Byte(Value);
    end;
  end;
end;

procedure TWatershedSegmentation.EuclideanDistance(X, Y: Integer; var Dist: Single);
var
  MinDist: Single;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
  DX, DY: Integer;
begin
  MinDist := MaxSingle;
  
  // Find minimum distance to a foreground pixel (non-zero)
  Neighbors := GetNeighbors(X, Y);
  for I := 0 to High(Neighbors) do
  begin
    Neighbor := Neighbors[I];
    if FImage[Neighbor.Value, Neighbor.Key] > 0 then
    begin
      DX := Neighbor.Key - X;
      DY := Neighbor.Value - Y;
      Dist := Sqrt(DX * DX + DY * DY);
      if Dist < MinDist then
        MinDist := Dist;
    end;
  end;

  if MinDist = MaxSingle then
    Dist := 0
  else
    Dist := MinDist;
end;

procedure TWatershedSegmentation.ComputeDistanceTransform1;
var
  X, Y: Integer;
  Changed: Boolean;
  MaxIterations: Integer;
  Iteration: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I : Integer;
  Dmax : SIngle ;
  Neighbor: TPair<Integer, Integer>;
begin

  // Initialize distance transform
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      if FImage[Y, X] > 0 then
        FDistance[Y, X] := 0
      else
        FDistance[Y, X] := 10000; // Large number
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



procedure TWatershedSegmentation.ComputeDistanceTransform;
var
  X, Y: Integer;
  Changed: Boolean;
  MaxIterations: Integer;
  Iteration: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
begin
  // Initialize distance transform
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      if FImage[Y, X] > 0 then
        FDistance[Y, X] := 0
      else
        FDistance[Y, X] := 10000; // Large number
    end;
  end;

// Initialize from GRADIENT, not binary image
 { for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      if FGradient[Y, X] = 0 then  // Use gradient, not binary threshold
        FDistance[Y, X] := 0       // Low gradient = particle interior
      else
        FDistance[Y, X] := 10000;  // High gradient = boundary
    end;
  end;}

  // Iterative distance computation
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
            if FDistance[Neighbor.Value, Neighbor.Key] > FDistance[Y, X] +1 then
            begin
              FDistance[Neighbor.Value, Neighbor.Key] := FDistance[Y, X] + 1;
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
        if FImage[Y, X] > 0 then
        begin
          Neighbors := GetNeighbors(X, Y);
          for I := 0 to High(Neighbors) do
          begin
            Neighbor := Neighbors[I];
            if FDistance[Neighbor.Value, Neighbor.Key] > FDistance[Y, X] +1 then
            begin
              FDistance[Neighbor.Value, Neighbor.Key] := FDistance[Y, X] + 1;
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

procedure TWatershedSegmentation.FindLocalMinima;
var
  X, Y: Integer;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
  IsMinimum: Boolean;
begin
  // Find local minima in the distance transform
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
          if FDistance[Neighbor.Value, Neighbor.Key] < FDistance[Y, X] then
          begin
            IsMinimum := False;
            Break;
          end;
        end;

        if IsMinimum then
          FMarkers[Y, X] := 1; // Mark as a local minimum
      end;
    end;
  end;
end;

procedure TWatershedSegmentation.LabelLocalMinima;
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

    // Label each local minimum with a unique label
    for Y := 0 to FHeight - 1 do
    begin
      for X := 0 to FWidth - 1 do
      begin
        if (FMarkers[Y, X] = 1) and (FLabels[Y, X] = 0) then
        begin
          // BFS from local minimum
          Queue.Enqueue(TPair<Integer, Integer>.Create(X, Y));
          FLabels[Y, X] := CurrentLabel;
          
          while Queue.Count > 0 do
          begin
            Current := Queue.Dequeue;
            Neighbors := GetNeighbors(Current.Key, Current.Value);
            
            for I := 0 to High(Neighbors) do
            begin
              Neighbor := Neighbors[I];
              if (FLabels[Neighbor.Value, Neighbor.Key] = 0) and
                 (FImage[Neighbor.Value, Neighbor.Key] > 0) then
              begin
                FLabels[Neighbor.Value, Neighbor.Key] := CurrentLabel;
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

procedure TWatershedSegmentation.WatershedFromMarkers;
var
  Queue: TQueue<TPair<Integer, Integer>>;
  Current: TPair<Integer, Integer>;
  Neighbors: TArray<TPair<Integer, Integer>>;
  I: Integer;
  Neighbor: TPair<Integer, Integer>;
  NeighborLabel: Integer;
begin
  Queue := TQueue<TPair<Integer, Integer>>.Create;
  try
    // Initialize queue with all labeled pixels
    for var Y := 0 to FHeight - 1 do
    begin
      for var X := 0 to FWidth - 1 do
      begin
        if FLabels[Y, X] > 0 then
          Queue.Enqueue(TPair<Integer, Integer>.Create(X, Y));
      end;
    end;

    // Watershed propagation
    while Queue.Count > 0 do
    begin
      Current := Queue.Dequeue;
      Neighbors := GetNeighbors(Current.Key, Current.Value);
      
      for I := 0 to High(Neighbors) do
      begin
        Neighbor := Neighbors[I];
        
        if FLabels[Neighbor.Value, Neighbor.Key] = 0 then
        begin
          NeighborLabel := FLabels[Current.Value, Current.Key];
          FLabels[Neighbor.Value, Neighbor.Key] := NeighborLabel;
          Queue.Enqueue(Neighbor);
        end;
      end;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TWatershedSegmentation.SegmentImage;
begin
  // Step 1: Compute gradient
  ComputeGradient;
  
  // Step 2: Compute distance transform
  ComputeDistanceTransform1;
  
  // Step 3: Find local minima (markers)
  FindLocalMinima;

  // Step 4: Label markers with unique IDs
  LabelLocalMinima;
  
  // Step 5: Watershed propagation
  WatershedFromMarkers;
end;

function TWatershedSegmentation.GetLabels: TArray<TArray<Integer>>;
begin
  Result := FLabels;
end;

function TWatershedSegmentation.GetSegmentCount: Integer;
var
  MaxLabel: Integer;
  X, Y: Integer;
begin
  MaxLabel := 0;
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      if FLabels[Y, X] > MaxLabel then
        MaxLabel := FLabels[Y, X];
    end;
  end;
  Result := MaxLabel;
end;

function TWatershedSegmentation.GetGradient: TArray<TArray<Byte>>;
begin
  Result := FGradient;
end;

end.