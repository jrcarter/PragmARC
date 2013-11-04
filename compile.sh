chmod 666 *.ali
gnatmake -c -m -gnatona -O2 -fstack-check compile_all.adb
rm compile_all.o
rm compile_all.ali
chmod 444 *.ali
