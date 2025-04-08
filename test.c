#define W Z
#define Z(ZARG) W(ZARG,2)
#define Y(YARG) Z(YARG)
#define X Y
X(X(1))