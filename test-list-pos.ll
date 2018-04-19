; ModuleID = 'Olleh'
source_filename = "Olleh"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

declare i8* @IntToS(i32)

declare void @InitializeRandom()

declare i8* @SConcat(i8*, i8*)

declare void @PrintCharLis(i8*)

define void @main() {
entry:
  call void @InitializeRandom()
  %a1 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32), i32 5))
  %a3 = getelementptr i8, i8* %a1, i32 0
  store i8 97, i8* %a3
  %a31 = getelementptr i8, i8* %a1, i32 1
  store i8 98, i8* %a31
  %a32 = getelementptr i8, i8* %a1, i32 2
  store i8 99, i8* %a32
  %a33 = getelementptr i8, i8* %a1, i32 3
  store i8 100, i8* %a33
  %a34 = getelementptr i8, i8* %a1, i32 4
  store i8 0, i8* %a34
  call void @PrintCharLis(i8* %a1)
  ret void
}

declare noalias i8* @malloc(i32)
