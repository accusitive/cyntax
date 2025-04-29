mkdir target/cyntax
cargo run && gcc -o /tmp/a.out harness.c ./target/cyntax/build.o && /tmp/a.out; echo $?
