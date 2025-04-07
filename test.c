#define W Z
#define Z(X) W(X,2)
#define Y(X) Z(X)
#define X Y

int test = X(1);
int main() {
	return test;
}