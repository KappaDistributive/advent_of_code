#include <stdio.h>
#include <stdlib.h>

# define FILENAME "../../data/2021/input_01.txt"

int number_of_lines_in_file(char* file_name) {
  FILE* file = fopen(file_name, "r");
  char ch;
  int number_of_lines = -1;

  if (file != NULL) {
    number_of_lines = 0;
    while ((ch=fgetc(file)) != EOF) {
      if (ch == '\n') {
        number_of_lines++;
      }
    }
  }

  fclose(file);

  return number_of_lines;
}

void parse_input(int* data, int number_of_lines) {

  FILE* file = fopen(FILENAME, "r");

  for (int index=0; index < number_of_lines; index++) {
    fscanf(file, "%d", &data[index]);
  }

  fclose(file);
}

int part_one(int* data, int number_of_lines) {
  int result = 0;
  for (int index = 1; index < number_of_lines; index++) {
    if (data[index -1] < data[index]) {
      result++;
    }
  }
  return result;
}

int part_two(int* data, int number_of_lines) {
  int result = 0;
  for (int index = 3; index < number_of_lines; index++) {
    if (data[index - 3] < data[index]) {
      result++;
    }
  }
  return result;
}


int main() {
  int number_of_lines = number_of_lines_in_file(FILENAME);
  printf("Number of lines: %d\n", number_of_lines);
  int* measurements = (int*) malloc(number_of_lines * sizeof(int));

  parse_input(measurements, number_of_lines);

  // for (int index = 0; index < number_of_lines; index++) {
  //   printf("%d\n", measurements[index]);
  // }
  printf("The answer to part one is: %d\n", part_one(measurements, number_of_lines));
  printf("The answer to part two is: %d\n", part_two(measurements, number_of_lines));


  free(measurements);
}
