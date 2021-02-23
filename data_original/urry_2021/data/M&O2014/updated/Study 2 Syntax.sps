USE ALL.
COMPUTE filter_$=(participantid ~= 194 & participantid ~= 237).
VARIABLE LABELS filter_$ 'participantid ~= 194 & participantid ~= 237 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA ZFindexA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCindexA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZFrawA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCrawA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZFindexW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCindexW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZFrawW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCrawW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

USE ALL.
COMPUTE filter_$=(participantid ~= 194 & participantid ~= 237 &notetype ~= 3).
VARIABLE LABELS filter_$ 'participantid ~= 194 & participantid ~= 237 &notetype ~= 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA ZFindexA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCindexA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZFrawA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCrawA BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZFindexW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCindexW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZFrawW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

UNIANOVA ZCrawW BY notetype whichtalk
  /RANDOM=whichtalk
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(notetype) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(whichtalk) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(notetype*whichtalk)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=notetype whichtalk notetype*whichtalk.

