#include <string>
#include <Arduino.h>
#include "LittleFS.h"

size_t part_one(const std::vector<int>& depths) {
  size_t increases{0};
  for (size_t index{1}; index < depths.size(); ++index) {
    if (depths[index] > depths[index-1]) {
      ++increases;
    }
  }
  return increases;
}

size_t part_two(const std::vector<int>& depths) {
  size_t increases{0};
  for (size_t index{3}; index < depths.size(); ++index) {
    if (depths[index] > depths[index-3]) {
      ++increases;
    }
  }
  return increases;
}

void setup() {
  Serial.begin(9600);
  if(!LittleFS.begin()){
    Serial.println("An Error has occurred while mounting LittleFS");
    return;
  }
  
  File file = LittleFS.open("/input_01.txt", "r");
  if(!file){
    Serial.println("Failed to open file for reading");
    return;
  }

  std::vector<int> depths; 
  while(file.available()){
    String line = file.readStringUntil('\n');
    depths.push_back(std::stoi(line.c_str()));
  }
  file.close();
  Serial.printf("Answer to part one: %zu\n", part_one(depths));
  Serial.printf("Answer to part two: %zu\n", part_two(depths));
}

void loop() {
  delay(500);
  // put your main code here, to run repeatedly:
}