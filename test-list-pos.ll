; ModuleID = 'Olleh'
source_filename = "Olleh"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @strlen(i8*)

declare i8* @IntToS(i32)

declare i32 @OllehRandom()

declare void @InitializeLocalGarbage()

declare void @CollectLocalGarbage()

declare void @InitializeRandom()

declare void @CollectLocalGarbageWithReturn(i8*)

declare i8* @SConcat(i8*, i8*)

declare void @ListOfIntsToString(i32*)

define void @main() {
entry:
  call void @InitializeRandom()
  call void @InitializeLocalGarbage()
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 5))
  %a1 = bitcast i8* %malloccall to i32*
  %a3 = getelementptr i32, i32* %a1, i32 0
  store i32 1, i32* %a3
  %a31 = getelementptr i32, i32* %a1, i32 1
  store i32 2, i32* %a31
  %a32 = getelementptr i32, i32* %a1, i32 2
  store i32 3, i32* %a32
  %a33 = getelementptr i32, i32* %a1, i32 3
  store i32 4, i32* %a33
  %a34 = getelementptr i32, i32* %a1, i32 4
  store i32 1073741823, i32* %a34
  call void @ListOfIntsToString(i32* %a1)
  call void @CollectLocalGarbage()
  ret void
}

declare noalias i8* @malloc(i32)
