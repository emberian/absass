#include <SoftwareSerial.h>
#include <string.h>
SoftwareSerial ass(2, 3);
char pbuf[32];

void setup() {
  Serial.begin(9600);
  ass.begin(9600);
  ass.listen();
}

void shuffle() {
  if (ass.available())
    Serial.write(ass.read());
   if (Serial.available())
    ass.write(Serial.read());
}

enum state {
  start, handshake, stream
} st = start;

bool do_handshake() {
  Serial.println("capturing CRAP-1 offgas...");
  ass.write('P');
  ass.write('A');
  ass.write('R');
  ass.write('C');
  char buf[5] = {0};
  int i = 0;
  while (i != 4) {
  while (!ass.available()) {
    delayMicroseconds(800);
  }
  buf[i++] = ass.read();
  sprintf(pbuf, "found %d\n", buf[i-1]);
  Serial.print(pbuf);
  }
  if (strncmp(buf, "CRAP", 4) == 0) {
    Serial.println("... ok!");
    return true;
  }
  Serial.write("... bad sync? ");
  Serial.write(buf);
  Serial.write('\n');
  return false;
}

void loop() {
  switch (st) {
    case start:
        Serial.println("Continue?");
        while(!Serial.available()) { }
        Serial.read();
        st = handshake;
        break;
    case handshake:
      if (do_handshake()) {
        st = stream;
      }
      break;
    case stream:
      shuffle();
      break;
  }
}
