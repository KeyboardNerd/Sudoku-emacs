#include <stdio.h>
#include <stdlib.h>
#define MAX_BOARD_SIZE = 10;

int board[9][9] = {0};
int check[3][10][10] = {0};
// get the corresponding block of position (col, row)
int getBlock(int col, int row){
  return col/3+row/3*3;
}
// check if the number can be put on (col, row)
int isValid(int col, int row, int number){
  return !(check[0][col][number] || check[1][row][number] || check[2][getBlock(col,row)][number]);
}
// check if the position in board
int isInBoard(int col, int row){
  return (col < 9 && row < 9 && col >= 0 && row >= 0);
}
// remove the number on (col, row)
int removeNum(int col, int row){
  int temp;
  if (isInBoard(col, row)){
    temp = board[col][row];
    check[0][col][temp] = 0;
    check[1][row][temp] = 0;
    check[2][getBlock(col,row)][temp] = 0;
    board[col][row] = 0;
    return 1;
  }
  return 0;
}
// put the number (number) on (col, row)
int putNum(int col, int row, int number){
  if (isValid(col,row,number) && isInBoard(col, row) && number < 10 && number > 0){
    removeNum(col,row);
    board[col][row] = number;
    check[0][col][number] = 1;
    check[1][row][number] = 1;
    check[2][getBlock(col,row)][number] = 1;
    return 1;
  }
  return 0;
}
// output the board to file
int board_save(char* fileName){
  FILE *file = fopen(fileName, "w");
  if (file == NULL)
    {
      perror("Error in board_save() while writing the file.\n");
      exit(EXIT_FAILURE);
    }
  int i = 0;
  for (; i < 9;i++){
    int j = 0;
    for (; j < 9; j++){
      fprintf(file, "%d ", board[i][j]);}
  }
  fclose(file);
  return 1;
}

// load the board from file
int board_load(char* fileName){
  char currentChar;
  FILE *file = fopen(fileName, "r");
  if (file == NULL)
    {
      perror("Error in board_load() while reading the file.\n");
      exit(EXIT_FAILURE);
    }
  int i = 0;
  // read the numbers into theBoard
  for (; i < 9; i++){
    int j = 0;
    for (; j < 9; j++){
      fscanf(file, "%d", &board[i][j]);
      check[0][i][board[i][j]] = 1;
      check[1][j][board[i][j]] = 1;
      check[2][getBlock(i,j)][board[i][j]] = 1;
    }
  }
  fclose(file);
  return 1;
}

void printBoard(){
  int i = 0;
  int j = 0;
  for (; i < 9; i++){
    j = 0;
    for (; j < 9; j++){
      printf("%d ",board[i][j]);
    }
    printf("\n");
  }
}

void solve(int col, int row, int &flag){
  int temp = 0;
  while (col < 9){
    while (row < 9){
      if (board[col][row] == 0){
        for (temp = 1; temp < 10; temp++){
          //printBoard();
          if (putNum(col,row,temp)){
            if (col == 8 && row == 8){
              
              flag = 1;
              return;
            }
            if (flag == 0) solve(col, row, flag);
            if (flag == 0) removeNum(col,row);
          }
        }
        if (col == 8 && row == 8){
          flag = 2;
        }
        return;
      }
      row++;
    }
    col++; 
    row = 0;
  }
  return;
}

int board_solve(){
  int resultFlag = 0;
  solve(0,0,resultFlag);
  return resultFlag;
}

int test_IO(){
  char* fileName = "saved";
  char* outName = "saved2";
  printf("Before loading\n");
  printBoard();
  board_load(fileName);
  printf("After loading\n");
  printBoard();
  board_save(outName);
  return 0;
}

int test_Solve(){
  char* fileName = "saved";
  board_load(fileName);
  board_solve();
  return 0;
}
void test(){
  int i = putNum(0,0,2);
  printf("%d\n", isValid(0,0,2));
  printf("put number\n");
  printBoard();
  printf("remove number\n");
  removeNum(0,0);
  printBoard();
}
int main(int argc, char* argv[]){
  if (argc != 3){
    perror("Error in main() : argc != 2");
    exit(EXIT_FAILURE);
  }
  board_load(argv[1]);
  int flag = board_solve();
  board_save(argv[2]);
  //test();
}
