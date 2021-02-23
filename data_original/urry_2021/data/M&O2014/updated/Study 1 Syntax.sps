
COMPUTE filter_$=(participant ~= 63).
VARIABLE LABELS filter_$ 'participant ~= 63 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
UNIANOVA ZFindexA BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZCindexA BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZFrawA BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZCrawA BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZFindexW BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZCindexW BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZFrawW BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.

UNIANOVA ZCrawW BY LapLong whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=LapLong whichtalk LapLong*whichtalk.
