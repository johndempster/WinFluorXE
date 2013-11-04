(*********************************************************************)
(*                                                                   *)
(* This file contains definitions for constants required for some    *)
(* of the NI-DAQ functions.                                          *)
(*                                                                   *)
(* You should use symbols defined here in your programs;  do not     *)
(* use the numerical values.                                         *)
(*                                                                   *)
(* See your NI-DAQ Function Reference Manual for details concerning  *)
(* use of constants defined here.                                    *)
(*                                                                   *)
(*********************************************************************)

unit NIDAQCNS;

interface
CONST	ND_ABOVE_HIGH_LEVEL           	 = 11020;
	ND_AC                         	 = 11025;
	ND_ACK_REQ_EXCHANGE_GR1       	 = 11030;
	ND_ACK_REQ_EXCHANGE_GR2       	 = 11035;
	ND_ACTIVE                     	 = 11037;
	ND_ADC_RESOLUTION             	 = 11040;
	ND_AI_CALDAC_COUNT            	 = 11050;
	ND_AI_CHANNEL_COUNT           	 = 11060;
	ND_AI_COUPLING                	 = 11055;
	ND_AI_FIFO_INTERRUPTS         	 = 11600;
	ND_ANALOG_FILTER              	 = 11065;
	ND_AO48XDC_SET_POWERUP_STATE  	 = 42100;
	ND_AO_CALDAC_COUNT            	 = 11070;
	ND_AO_CHANNEL_COUNT           	 = 11080;
	ND_AO_EXT_REF_CAPABLE         	 = 11090;
	ND_AO_UNIPOLAR_CAPABLE        	 = 11095;
	ND_ARM                        	 = 11100;
	ND_ARMED                      	 = 11200;
	ND_ATC_OUT                    	 = 11250;
	ND_ATTENUATION                	 = 11260;
	ND_AUTOINCREMENT_COUNT        	 = 11300;
	ND_AUTOMATIC                  	 = 11400;
	ND_AVAILABLE_POINTS           	 = 11500;

	ND_BASE_ADDRESS               	 = 12100;
	ND_BELOW_LOW_LEVEL            	 = 12130;
	ND_BOARD_CLOCK                	 = 12170;
	ND_BUFFERED_EVENT_CNT         	 = 12200;
	ND_BUFFERED_PERIOD_MSR        	 = 12300;
	ND_BUFFERED_PULSE_WIDTH_MSR   	 = 12400;
	ND_BUFFERED_SEMI_PERIOD_MSR   	 = 12500;
	ND_BURST                      	 = 12600;
	ND_BURST_INTERVAL             	 = 12700;

	ND_CAL_CONST_AUTO_LOAD        	 = 13050;
	ND_CALIBRATION_ENABLE         	 = 13055;
	ND_CALIBRATION_FRAME_SIZE     	 = 13060;
	ND_CALIBRATION_FRAME_PTR      	 = 13065;
	ND_CJ_TEMP                    	 = ($8000);
	ND_CALGND                     	 = ($8001);
	ND_CLEAN_UP                   	 = 13100;
	ND_CLOCK_REVERSE_MODE_GR1     	 = 13120;
	ND_CLOCK_REVERSE_MODE_GR2     	 = 13130;
	ND_CONFIG_MEMORY_SIZE         	 = 13150;
	ND_CONTINUOUS                 	 = 13160;
	ND_COUNT                      	 = 13200;

	ND_COUNTER_0                  	 = 13300;
	ND_COUNTER_1                  	 = 13400;
	ND_COUNTER_2                  	 = 13310;
	ND_COUNTER_3                  	 = 13320;
	ND_COUNTER_4                  	 = 13330;
	ND_COUNTER_5                  	 = 13340;
	ND_COUNTER_6                  	 = 13350;
	ND_COUNTER_7                  	 = 13360;

	ND_COUNTER_1_SOURCE           	 = 13430;
	ND_COUNT_AVAILABLE            	 = 13450;
	ND_COUNT_DOWN                 	 = 13465;
	ND_COUNT_UP                   	 = 13485;
	ND_COUNT_1                    	 = 13500;
	ND_COUNT_2                    	 = 13600;
	ND_COUNT_3                    	 = 13700;
	ND_COUNT_4                    	 = 13800;
	ND_CURRENT_OUTPUT             	 = 40200;

	ND_DAC_RESOLUTION             	 = 13950;
	ND_DATA_TRANSFER_CONDITION    	 = 13960;
	ND_DATA_XFER_MODE_AI          	 = 14000;
	ND_DATA_XFER_MODE_AO_GR1      	 = 14100;
	ND_DATA_XFER_MODE_AO_GR2      	 = 14200;
	ND_DATA_XFER_MODE_DIO_GR1     	 = 14300;
	ND_DATA_XFER_MODE_DIO_GR2     	 = 14400;
	ND_DATA_XFER_MODE_DIO_GR3     	 = 14500;
	ND_DATA_XFER_MODE_DIO_GR4     	 = 14600;
	ND_DATA_XFER_MODE_DIO_GR5     	 = 14700;
	ND_DATA_XFER_MODE_DIO_GR6     	 = 14800;
	ND_DATA_XFER_MODE_DIO_GR7     	 = 14900;
	ND_DATA_XFER_MODE_DIO_GR8     	 = 15000;

	ND_DATA_XFER_MODE_GPCTR0      	 = 15100;
	ND_DATA_XFER_MODE_GPCTR1      	 = 15200;
	ND_DATA_XFER_MODE_GPCTR2      	 = 15110;
	ND_DATA_XFER_MODE_GPCTR3      	 = 15120;
	ND_DATA_XFER_MODE_GPCTR4      	 = 15130;
	ND_DATA_XFER_MODE_GPCTR5      	 = 15140;
	ND_DATA_XFER_MODE_GPCTR6      	 = 15150;
	ND_DATA_XFER_MODE_GPCTR7      	 = 15160;
	ND_DATA_XFER_MODE_GPCTR8      	 = 15165;
	ND_DATA_XFER_MODE_GPCTR9      	 = 15170;
	ND_DATA_XFER_MODE_GPCTR10     	 = 15175;
	ND_DATA_XFER_MODE_GPCTR11     	 = 15180;

	ND_DC                         	 = 15250;
	ND_DDS_BUFFER_SIZE            	 = 15255;
	ND_DEVICE_NAME                	 = 15260;
	ND_DEVICE_POWER               	 = 15270;
	ND_DEVICE_SERIAL_NUMBER       	 = 15280;
	ND_DEVICE_STATE_DURING_SUSPEND_MODE	 = 15290;
	ND_DEVICE_TYPE_CODE           	 = 15300;
	ND_DIGITAL_FILTER             	 = 15350;
	ND_DIGITAL_RESTART            	 = 15375;
	ND_DIO128_GET_PORT_THRESHOLD  	 = 41200;
	ND_DIO128_SELECT_INPUT_PORT   	 = 41100;
	ND_DIO128_SET_PORT_THRESHOLD  	 = 41300;
	ND_DISABLED                   	 = 15400;
	ND_DISARM                     	 = 15450;
	ND_DIVIDE_DOWN_SAMPLING_SUPPORTED	 = 15475;
	ND_DMA_A_LEVEL                	 = 15500;
	ND_DMA_B_LEVEL                	 = 15600;
	ND_DMA_C_LEVEL                	 = 15700;
	ND_DONE                       	 = 15800;
	ND_DONT_CARE                  	 = 15900;
	ND_DONT_KNOW                  	 = 15950;

	ND_EDGE_SENSITIVE             	 = 16000;
	ND_ENABLED                    	 = 16050;
	ND_END                        	 = 16055;
	ND_EXTERNAL                   	 = 16060;
	ND_EXTERNAL_CALIBRATE         	 = 16100;

	ND_FACTORY_CALIBRATION_EQUIP  	 = 16210;
	ND_FACTORY_EEPROM_AREA        	 = 16220;
	ND_FIFO_EMPTY                 	 = 16230;
	ND_FIFO_HALF_FULL_OR_LESS     	 = 16240;
	ND_FIFO_HALF_FULL_OR_LESS_UNTIL_FULL	 = 16245;
	ND_FIFO_NOT_FULL              	 = 16250;
	ND_FIFO_TRANSFER_COUNT        	 = 16260;
	ND_FILTER_CORRECTION_FREQ     	 = 16300;
	ND_FOREGROUND                 	 = 16350;
	ND_FREQ_OUT                   	 = 16400;
	ND_FSK                        	 = 16500;
	ND_EDGE_BASED_FSK             	 = 16500;

	ND_GATE                       	 = 17100;
	ND_GATE_POLARITY              	 = 17200;

	ND_GPCTR0_GATE                	 = 17300;
	ND_GPCTR0_OUTPUT              	 = 17400;
	ND_GPCTR0_SOURCE              	 = 17500;

	ND_GPCTR1_GATE                	 = 17600;
	ND_GPCTR1_OUTPUT              	 = 17700;
	ND_GPCTR1_SOURCE              	 = 17800;

	ND_GPCTR2_GATE                	 = 17320;
	ND_GPCTR2_OUTPUT              	 = 17420;
	ND_GPCTR2_SOURCE              	 = 17520;

	ND_GPCTR3_GATE                	 = 17330;
	ND_GPCTR3_OUTPUT              	 = 17430;
	ND_GPCTR3_SOURCE              	 = 17530;

	ND_GPCTR4_GATE                	 = 17340;
	ND_GPCTR4_OUTPUT              	 = 17440;
	ND_GPCTR4_SOURCE              	 = 17540;

	ND_GPCTR5_GATE                	 = 17350;
	ND_GPCTR5_OUTPUT              	 = 17450;
	ND_GPCTR5_SOURCE              	 = 17550;

	ND_GPCTR6_GATE                	 = 17360;
	ND_GPCTR6_OUTPUT              	 = 17460;
	ND_GPCTR6_SOURCE              	 = 17660;

	ND_GPCTR7_GATE                	 = 17370;
	ND_GPCTR7_OUTPUT              	 = 17470;
	ND_GPCTR7_SOURCE              	 = 17570;





	ND_GROUND_DAC_REFERENCE       	 = 17900;

	ND_HARDWARE                   	 = 18000;
	ND_HI_RES_SAMPLING            	 = 18020;
	ND_HIGH                       	 = 18050;
	ND_HIGH_HYSTERESIS            	 = 18080;
	ND_HIGH_TO_LOW                	 = 18100;
	ND_HW_ANALOG_TRIGGER          	 = 18900;

	ND_IMPEDANCE                  	 = 19000;
	ND_INACTIVE                   	 = 19010;
	ND_INITIAL_COUNT              	 = 19100;
	ND_INIT_PLUGPLAY_DEVICES      	 = 19110;
	ND_INSIDE_REGION              	 = 19150;
	ND_INTERNAL                   	 = 19160;
	ND_INTERNAL_100_KHZ           	 = 19200;
	ND_INTERNAL_10_MHZ            	 = 19300;
	ND_INTERNAL_1250_KHZ          	 = 19320;
	ND_INTERNAL_20_MHZ            	 = 19400;
	ND_INTERNAL_25_MHZ            	 = 19410;
	ND_INTERNAL_2500_KHZ          	 = 19420;
	ND_INTERNAL_5_MHZ             	 = 19450;
	ND_INTERNAL_7160_KHZ          	 = 19460;
	ND_INTERNAL_TIMER             	 = 19500;
	ND_INTERRUPTS                 	 = 19600;
	ND_INTERRUPT_A_LEVEL          	 = 19700;
	ND_INTERRUPT_B_LEVEL          	 = 19800;
	ND_INTERRUPT_TRIGGER_MODE     	 = 19850;
	ND_IN_CHANNEL_CLOCK_TIMEBASE  	 = 19900;
	ND_IN_CHANNEL_CLOCK_TB_POL    	 = 20000;
	ND_IN_CONVERT                 	 = 20100;
	ND_IN_CONVERT_POL             	 = 20200;
	ND_IN_DATA_FIFO_SIZE          	 = 20250;
	ND_IN_EXTERNAL_GATE           	 = 20300;
	ND_IN_EXTERNAL_GATE_POL       	 = 20400;
	ND_IN_SCAN_CLOCK_TIMEBASE     	 = 20500;
	ND_IN_SCAN_CLOCK_TB_POL       	 = 20600;
	ND_IN_SCAN_IN_PROG            	 = 20650;
	ND_IN_SCAN_START              	 = 20700;
	ND_IN_SCAN_START_POL          	 = 20800;
	ND_IN_START_TRIGGER           	 = 20900;
	ND_IN_START_TRIGGER_POL       	 = 21000;
	ND_IN_STOP_TRIGGER            	 = 21100;
	ND_IN_STOP_TRIGGER_POL        	 = 21200;
	ND_INT_AI_GND                 	 = 21210;
	ND_INT_AO_CH_0                	 = 21230;
	ND_INT_AO_CH_0_VS_REF_5V      	 = 21235;
	ND_INT_AO_CH_1                	 = 21240;
	ND_INT_AO_CH_1_VS_AO_CH_0     	 = 21245;
	ND_INT_AO_CH_1_VS_REF_5V      	 = 21250;
	ND_INT_AO_CH_2                	 = 21220;
	ND_INT_AO_CH_3                	 = 21221;
	ND_INT_AO_CH_4                	 = 21222;
	ND_INT_AO_CH_5                	 = 21223;
	ND_INT_AO_CH_6                	 = 21224;
	ND_INT_AO_CH_7                	 = 21225;
	ND_INT_AO_GND                 	 = 21260;
	ND_INT_AO_GND_VS_AI_GND       	 = 21265;
	ND_INT_CM_REF_5V              	 = 21270;
	ND_INT_DEV_TEMP               	 = 21280;
	ND_INT_REF_5V                 	 = 21290;
	ND_INT_REF_EXTERN             	 = 21296;
	ND_INT_CAL_BUS                	 = 21295;
	ND_INT_MUX_BUS                	 = 21305;

	ND_INT_AI_GND_AMP_0           	 = 21211;
	ND_INT_AI_GND_AMP_1           	 = 21212;
	ND_INT_AI_GND_AMP_2           	 = 21213;
	ND_INT_AI_GND_AMP_3           	 = 21214;
	ND_INT_AO_CH_0_AMP_0          	 = 21231;
	ND_INT_AO_CH_0_AMP_1          	 = 21232;
	ND_INT_AO_CH_0_AMP_2          	 = 21233;
	ND_INT_AO_CH_0_AMP_3          	 = 21234;
	ND_INT_AO_CH_1_AMP_0          	 = 21241;
	ND_INT_AO_CH_1_AMP_1          	 = 21242;
	ND_INT_AO_CH_1_AMP_2          	 = 21243;
	ND_INT_AO_CH_1_AMP_3          	 = 21244;
	ND_INT_AO_CH_0_VS_REF_AMP_0   	 = 21236;
	ND_INT_AO_CH_0_VS_REF_AMP_1   	 = 21237;
	ND_INT_AO_CH_0_VS_REF_AMP_2   	 = 21238;
	ND_INT_AO_CH_0_VS_REF_AMP_3   	 = 21239;
	ND_INT_AO_CH_1_VS_REF_AMP_0   	 = 21251;
	ND_INT_AO_CH_1_VS_REF_AMP_1   	 = 21252;
	ND_INT_AO_CH_1_VS_REF_AMP_2   	 = 21253;
	ND_INT_AO_CH_1_VS_REF_AMP_3   	 = 21254;
	ND_INT_AO_GND_VS_AI_GND_AMP_0 	 = 21266;
	ND_INT_AO_GND_VS_AI_GND_AMP_1 	 = 21267;
	ND_INT_AO_GND_VS_AI_GND_AMP_2 	 = 21268;
	ND_INT_AO_GND_VS_AI_GND_AMP_3 	 = 21269;
	ND_INT_CM_REF_AMP_0           	 = 21271;
	ND_INT_CM_REF_AMP_1           	 = 21272;
	ND_INT_CM_REF_AMP_2           	 = 21273;
	ND_INT_CM_REF_AMP_3           	 = 21274;
	ND_INT_REF_AMP_0              	 = 21291;
	ND_INT_REF_AMP_1              	 = 21292;
	ND_INT_REF_AMP_2              	 = 21293;
	ND_INT_REF_AMP_3              	 = 21294;

	ND_INTERRUPT_EVERY_SAMPLE     	 = 11700;
	ND_INTERRUPT_HALF_FIFO        	 = 11800;
	ND_IO_CONNECTOR               	 = 21300;

	ND_LEVEL_SENSITIVE            	 = 24000;
	ND_LINK_COMPLETE_INTERRUPTS   	 = 24010;
	ND_LOW                        	 = 24050;
	ND_LOW_HYSTERESIS             	 = 24080;
	ND_LOW_TO_HIGH                	 = 24100;
	ND_LPT_DEVICE_MODE            	 = 24200;

	ND_MARKER                     	 = 24500;
	ND_MARKER_QUANTUM             	 = 24550;
	ND_MAX_ARB_SEQUENCE_LENGTH    	 = 24600;
	ND_MAX_FUNC_SEQUENCE_LENGTH   	 = 24610;
	ND_MAX_LOOP_COUNT             	 = 24620;
	ND_MAX_NUM_WAVEFORMS          	 = 24630;
	ND_MAX_SAMPLE_RATE            	 = 24640;
	ND_MAX_WFM_SIZE               	 = 24650;
	ND_MEMORY_TRANSFER_WIDTH      	 = 24700;
	ND_MIN_SAMPLE_RATE            	 = 24800;
	ND_MIN_WFM_SIZE               	 = 24810;

	ND_NEGATIVE                   	 = 26100;
	ND_NEW                        	 = 26190;
	ND_NI_DAQ_SW_AREA             	 = 26195;
	ND_NO                         	 = 26200;
	ND_NO_STRAIN_GAUGE            	 = 26225;
	ND_NO_TRACK_AND_HOLD          	 = 26250;
	ND_NONE                       	 = 26300;
	ND_NOT_APPLICABLE             	 = 26400;
	ND_NUMBER_DIG_PORTS           	 = 26500;

	ND_OFF                        	 = 27010;
	ND_OFFSET                     	 = 27020;
	ND_ON                         	 = 27050;
	ND_OTHER                      	 = 27060;
	ND_OTHER_GPCTR_OUTPUT         	 = 27300;
	ND_OTHER_GPCTR_TC             	 = 27400;
	ND_OUT_DATA_FIFO_SIZE         	 = 27070;
	ND_OUT_EXTERNAL_GATE          	 = 27080;
	ND_OUT_EXTERNAL_GATE_POL      	 = 27082;
	ND_OUT_START_TRIGGER          	 = 27100;
	ND_OUT_START_TRIGGER_POL      	 = 27102;
	ND_OUT_UPDATE                 	 = 27200;
	ND_OUT_UPDATE_POL             	 = 27202;
	ND_OUT_UPDATE_CLOCK_TIMEBASE  	 = 27210;
	ND_OUT_UPDATE_CLOCK_TB_POL    	 = 27212;
	ND_OUTPUT_ENABLE              	 = 27220;
	ND_OUTPUT_MODE                	 = 27230;
	ND_OUTPUT_POLARITY            	 = 27240;
	ND_OUTPUT_STATE               	 = 27250;
	ND_OUTPUT_TYPE                	 = 40000;

	ND_DIGITAL_PATTERN_GENERATION 	 = 28030;
	ND_PAUSE                      	 = 28040;
	ND_PAUSE_ON_HIGH              	 = 28045;
	ND_PAUSE_ON_LOW               	 = 28050;
	ND_PFI_0                      	 = 28100;
	ND_PFI_1                      	 = 28200;
	ND_PFI_2                      	 = 28300;
	ND_PFI_3                      	 = 28400;
	ND_PFI_4                      	 = 28500;
	ND_PFI_5                      	 = 28600;
	ND_PFI_6                      	 = 28700;
	ND_PFI_7                      	 = 28800;
	ND_PFI_8                      	 = 28900;
	ND_PFI_9                      	 = 29000;
	ND_PFI_10                     	 = 50280;
	ND_PFI_11                     	 = 50290;
	ND_PFI_12                     	 = 50300;
	ND_PFI_13                     	 = 50310;
	ND_PFI_14                     	 = 50320;
	ND_PFI_15                     	 = 50330;
	ND_PFI_16                     	 = 50340;
	ND_PFI_17                     	 = 50350;
	ND_PFI_18                     	 = 50360;
	ND_PFI_19                     	 = 50370;
	ND_PFI_20                     	 = 50380;
	ND_PFI_21                     	 = 50390;
	ND_PFI_22                     	 = 50400;
	ND_PFI_23                     	 = 50410;
	ND_PFI_24                     	 = 50420;
	ND_PFI_25                     	 = 50430;
	ND_PFI_26                     	 = 50440;
	ND_PFI_27                     	 = 50450;
	ND_PFI_28                     	 = 50460;
	ND_PFI_29                     	 = 50470;
	ND_PFI_30                     	 = 50480;
	ND_PFI_31                     	 = 50490;
	ND_PFI_32                     	 = 50500;
	ND_PFI_33                     	 = 50510;
	ND_PFI_34                     	 = 50520;
	ND_PFI_35                     	 = 50530;
	ND_PFI_36                     	 = 50540;
	ND_PFI_37                     	 = 50550;
	ND_PFI_38                     	 = 50560;
	ND_PFI_39                     	 = 50570;

	ND_PLL_REF_FREQ               	 = 29010;
	ND_PLL_REF_SOURCE             	 = 29020;
	ND_PRE_ARM                    	 = 29050;
	ND_POSITIVE                   	 = 29100;
	ND_PREPARE                    	 = 29200;
	ND_PROGRAM                    	 = 29300;
	ND_PULSE                      	 = 29350;
	ND_PULSE_SOURCE               	 = 29500;
	ND_PULSE_TRAIN_GNR            	 = 29600;
	ND_PXI_BACKPLANE_CLOCK        	 = 29900;

	ND_REGLITCH                   	 = 31000;
	ND_RESERVED                   	 = 31100;
	ND_RESET                      	 = 31200;
	ND_RESUME                     	 = 31250;
	ND_RETRIG_PULSE_GNR           	 = 31300;
	ND_REVISION                   	 = 31350;
	ND_RTSI_0                     	 = 31400;
	ND_RTSI_1                     	 = 31500;
	ND_RTSI_2                     	 = 31600;
	ND_RTSI_3                     	 = 31700;
	ND_RTSI_4                     	 = 31800;
	ND_RTSI_5                     	 = 31900;
	ND_RTSI_6                     	 = 32000;
	ND_RTSI_CLOCK                 	 = 32100;

	ND_SCANCLK                    	 = 32400;
	ND_SCANCLK_LINE               	 = 32420;
	ND_SC_2040_MODE               	 = 32500;
	ND_SC_2043_MODE               	 = 32600;
	ND_SELF_CALIBRATE             	 = 32700;
	ND_SET_DEFAULT_LOAD_AREA      	 = 32800;
	ND_RESTORE_FACTORY_CALIBRATION	 = 32810;
	ND_SET_POWERUP_STATE          	 = 42100;
	ND_SIMPLE_EVENT_CNT           	 = 33100;
	ND_SINGLE                     	 = 33150;
	ND_SINGLE_PERIOD_MSR          	 = 33200;
	ND_SINGLE_PULSE_GNR           	 = 33300;
	ND_SINGLE_PULSE_WIDTH_MSR     	 = 33400;
	ND_SINGLE_TRIG_PULSE_GNR      	 = 33500;
	ND_SOURCE                     	 = 33700;
	ND_SOURCE_POLARITY            	 = 33800;
	ND_STABLE_10_MHZ              	 = 33810;
	ND_STEPPED                    	 = 33825;
	ND_STRAIN_GAUGE               	 = 33850;
	ND_STRAIN_GAUGE_EX0           	 = 33875;
	ND_SUB_REVISION               	 = 33900;
	ND_SYNC_DUTY_CYCLE_HIGH       	 = 33930;
	ND_SYNC_OUT                   	 = 33970;

	ND_TC_REACHED                 	 = 34100;
	ND_THE_AI_CHANNEL             	 = 34400;
	ND_TOGGLE                     	 = 34700;
	ND_TOGGLE_GATE                	 = 34800;
	ND_TRACK_AND_HOLD             	 = 34850;
	ND_TRIG_PULSE_WIDTH_MSR       	 = 34900;
	ND_TRIGGER_SOURCE             	 = 34930;
	ND_TRIGGER_MODE               	 = 34970;

	ND_UI2_TC                     	 = 35100;
	ND_UP_DOWN                    	 = 35150;
	ND_UP_TO_1_DMA_CHANNEL        	 = 35200;
	ND_UP_TO_2_DMA_CHANNELS       	 = 35300;
	ND_USE_CAL_CHAN               	 = 36000;
	ND_USE_AUX_CHAN               	 = 36100;
	ND_USER_EEPROM_AREA           	 = 37000;
	ND_USER_EEPROM_AREA_2         	 = 37010;
	ND_USER_EEPROM_AREA_3         	 = 37020;
	ND_USER_EEPROM_AREA_4         	 = 37030;
	ND_USER_EEPROM_AREA_5         	 = 37040;

	ND_DSA_RTSI_CLOCK_AD          	 = 44000;
	ND_DSA_RTSI_CLOCK_DA          	 = 44010;
	ND_DSA_OUTPUT_TRIGGER         	 = 44020;
	ND_DSA_INPUT_TRIGGER          	 = 44030;
	ND_DSA_SHARC_TRIGGER          	 = 44040;
	ND_DSA_ANALOG_TRIGGER         	 = 44050;
	ND_DSA_HOST_TRIGGER           	 = 44060;
	ND_DSA_EXTERNAL_DIGITAL_TRIGGER	 = 44070;

	ND_VOLTAGE_OUTPUT             	 = 40100;
	ND_VOLTAGE_REFERENCE          	 = 38000;

	ND_VXI_SC                     	 = ($2000);
	ND_PXI_SC                     	 = ($2010);
	ND_VXIMIO_SET_ALLOCATE_MODE   	 = 43100;
	ND_VXIMIO_USE_ONBOARD_MEMORY_AI	 = 43500;
	ND_VXIMIO_USE_ONBOARD_MEMORY_AO	 = 43600;
	ND_VXIMIO_USE_ONBOARD_MEMORY_GPCTR	 = 43700;
	ND_VXIMIO_USE_PC_MEMORY_AI    	 = 43200;
	ND_VXIMIO_USE_PC_MEMORY_AO    	 = 43300;
	ND_VXIMIO_USE_PC_MEMORY_GPCTR 	 = 43400;

	ND_WFM_QUANTUM                	 = 45000;

	ND_YES                        	 = 39100;
	ND_3V_LEVEL                   	 = 43450;

	ND_WRITE_MARK                 	 = 50000;
	ND_READ_MARK                  	 = 50010;
	ND_BUFFER_START               	 = 50020;
	ND_TRIGGER_POINT              	 = 50025;
	ND_BUFFER_MODE                	 = 50030;
	ND_DOUBLE                     	 = 50050;
	ND_QUADRATURE_ENCODER_X1      	 = 50070;
	ND_QUADRATURE_ENCODER_X2      	 = 50080;
	ND_QUADRATURE_ENCODER_X4      	 = 50090;
	ND_TWO_PULSE_COUNTING         	 = 50100;
	ND_LINE_FILTER                	 = 50110;
	ND_SYNCHRONIZATION            	 = 50120;
	ND_5_MICROSECONDS             	 = 50130;
	ND_1_MICROSECOND              	 = 50140;
	ND_500_NANOSECONDS            	 = 50150;
	ND_100_NANOSECONDS            	 = 50160;
	ND_1_MILLISECOND              	 = 50170;
	ND_10_MILLISECONDS            	 = 50180;
	ND_100_MILLISECONDS           	 = 50190;


	ND_OTHER_GPCTR_SOURCE         	 = 50580;
	ND_OTHER_GPCTR_GATE           	 = 50590;
	ND_AUX_LINE                   	 = 50600;
	ND_AUX_LINE_POLARITY          	 = 50610;
	ND_TWO_SIGNAL_EDGE_SEPARATION_MSR	 = 50630;
	ND_BUFFERED_TWO_SIGNAL_EDGE_SEPARATION_MSR	 = 50640;
	ND_SWITCH_CYCLE               	 = 50650;
	ND_INTERNAL_MAX_TIMEBASE      	 = 50660;
	ND_PRESCALE_VALUE             	 = 50670;
	ND_MAX_PRESCALE               	 = 50690;
	ND_INTERNAL_LINE_0            	 = 50710;
	ND_INTERNAL_LINE_1            	 = 50720;
	ND_INTERNAL_LINE_2            	 = 50730;
	ND_INTERNAL_LINE_3            	 = 50740;
	ND_INTERNAL_LINE_4            	 = 50750;
	ND_INTERNAL_LINE_5            	 = 50760;
	ND_INTERNAL_LINE_6            	 = 50770;
	ND_INTERNAL_LINE_7            	 = 50780;
	ND_INTERNAL_LINE_8            	 = 50790;
	ND_INTERNAL_LINE_9            	 = 50800;
	ND_INTERNAL_LINE_10           	 = 50810;
	ND_INTERNAL_LINE_11           	 = 50820;
	ND_INTERNAL_LINE_12           	 = 50830;
	ND_INTERNAL_LINE_13           	 = 50840;
	ND_INTERNAL_LINE_14           	 = 50850;
	ND_INTERNAL_LINE_15           	 = 50860;
	ND_INTERNAL_LINE_16           	 = 50862;
	ND_INTERNAL_LINE_17           	 = 50864;
	ND_INTERNAL_LINE_18           	 = 50866;
	ND_INTERNAL_LINE_19           	 = 50868;
	ND_INTERNAL_LINE_20           	 = 50870;
	ND_INTERNAL_LINE_21           	 = 50872;
	ND_INTERNAL_LINE_22           	 = 50874;
	ND_INTERNAL_LINE_23           	 = 50876;





	ND_START_TRIGGER              	 = 51150;
	ND_START_TRIGGER_POLARITY     	 = 51151;



	ND_COUNTING_SYNCHRONOUS       	 = 51200;
	ND_SYNCHRONOUS                	 = 51210;
	ND_ASYNCHRONOUS               	 = 51220;
	ND_CONFIGURABLE_FILTER        	 = 51230;
	ND_ENCODER_TYPE               	 = 51240;
	ND_Z_INDEX_ACTIVE             	 = 51250;
	ND_Z_INDEX_VALUE              	 = 51260;
	ND_SNAPSHOT                   	 = 51270;
	ND_POSITION_MSR               	 = 51280;
	ND_BUFFERED_POSITION_MSR      	 = 51290;
	ND_SAVED_COUNT                	 = 51300;
	ND_READ_MARK_H_SNAPSHOT       	 = 51310;
	ND_READ_MARK_L_SNAPSHOT       	 = 51320;
	ND_WRITE_MARK_H_SNAPSHOT      	 = 51330;
	ND_WRITE_MARK_L_SNAPSHOT      	 = 51340;
	ND_BACKLOG_H_SNAPSHOT         	 = 51350;
	ND_BACKLOG_L_SNAPSHOT         	 = 51360;
	ND_ARMED_SNAPSHOT             	 = 51370;
	ND_EDGE_GATED_FSK             	 = 51371;
	ND_SIMPLE_GATED_EVENT_CNT     	 = 51372;

	ND_VIDEO_TYPE                 	 = 51380;
	ND_PAL_B                      	 = 51390;
	ND_PAL_G                      	 = 51400;
	ND_PAL_H                      	 = 51410;
	ND_PAL_I                      	 = 51420;
	ND_PAL_D                      	 = 51430;
	ND_PAL_N                      	 = 51440;
	ND_PAL_M                      	 = 51450;
	ND_NTSC_M                     	 = 51460;
	ND_COUNTER_TYPE               	 = 51470;
	ND_NI_TIO                     	 = 51480;
	ND_AM9513                     	 = 51490;
	ND_STC                        	 = 51500;
	ND_8253                       	 = 51510;
	ND_A_HIGH_B_HIGH              	 = 51520;
	ND_A_HIGH_B_LOW               	 = 51530;
	ND_A_LOW_B_HIGH               	 = 51540;
	ND_A_LOW_B_LOW                	 = 51550;
	ND_Z_INDEX_RELOAD_PHASE       	 = 51560;
	ND_UPDOWN_LINE                	 = 51570;
	ND_DEFAULT_PFI_LINE           	 = 51580;
	ND_BUFFER_SIZE                	 = 51590;
	ND_ELEMENT_SIZE               	 = 51600;
	ND_NUMBER_GP_COUNTERS         	 = 51610;
	ND_BUFFERED_TIME_STAMPING     	 = 51620;
	ND_TIME_0_DATA_32             	 = 51630;
	ND_TIME_8_DATA_24             	 = 51640;
	ND_TIME_16_DATA_16            	 = 51650;
	ND_TIME_24_DATA_8             	 = 51660;
	ND_TIME_32_DATA_32            	 = 51670;
	ND_TIME_48_DATA_16            	 = 51680;
	ND_ABSOLUTE                   	 = 51690;
	ND_RELATIVE                   	 = 51700;
	ND_TIME_DATA_SIZE             	 = 51710;
	ND_TIME_FORMAT                	 = 51720;
	ND_HALT_ON_OVERFLOW           	 = 51730;
	ND_OVERLAY_RTSI_ON_PFI_LINES  	 = 51740;
	ND_STOP_TRIGGER               	 = 51750;
	ND_TS_INPUT_MODE              	 = 51760;
	ND_BOTH_EDGES                 	 = 51770;

	ND_CLOCK_0                    	 = 51780;
	ND_CLOCK_1                    	 = 51790;
	ND_CLOCK_2                    	 = 51800;
	ND_CLOCK_3                    	 = 51810;
	ND_SYNCHRONIZATION_LINE       	 = 51820;
	ND_TRANSFER_METHOD            	 = 51830;
	ND_SECONDS                    	 = 51840;
	ND_PRECISION                  	 = 51850;
	ND_NANO_SECONDS               	 = 51860;
	ND_SYNCHRONIZATION_METHOD     	 = 51870;
	ND_PULSE_PER_SECOND           	 = 51880;
	ND_IRIG_B                     	 = 51890;
	ND_SIMPLE_TIME_MSR            	 = 51900;
	ND_SINGLE_TIME_MSR            	 = 51910;
	ND_BUFFERED_TIME_MSR          	 = 51920;
	ND_DMA                        	 = 51930;


implementation

end.
