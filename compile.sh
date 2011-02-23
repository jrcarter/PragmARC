chmod 666 *.ali
gnatmake -c -m -gnaton -O2 -fstack-check -gnat95 compile_all.adb
rm compile_all.o
rm compile_all.ali
chmod 444 *.ali
