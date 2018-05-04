; ModuleID = 'Olleh'
source_filename = "Olleh"

%mapt = type { i8*, i32, %mapt* }
%charmapt = type { i8, i32, %charmapt* }

@dictionary = global %mapt* null
@letterScores = global %charmapt* null
@lis_2 = global i8** null
@lis = global i8* null
@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@0 = private unnamed_addr constant [4 x i8] c"abc\00"

declare i32 @printf(i8*, ...)

declare i8* @IntToS(i32)

declare i8* @SConcat(i8*, i8*)

declare void @PrintCharLis(i8*)

declare void @PrintStringmap(%mapt*)

declare void @PrintCharmap(%charmapt*)

declare void @PrintListList(i8**)

declare i32 @strlen(i8*)

declare i32 @ListlistgetLength(i8**)

declare i32 @CharmapgetLength(%charmapt*)

declare i32 @StringmapgetLength(%mapt*)

declare i32 @Charmapget(%charmapt*, i8)

declare i32 @Stringmapget(%mapt*, i8*)

declare i8 @Charmapgeti(%charmapt*, i32)

declare i8* @Stringmapgeti(%mapt*, i32)

declare i8 @Charlistget(i8*, i32)

declare i8* @Listlistget(i8**, i32)

declare %charmapt* @Charmapset(%charmapt*, i8, i32)

declare %mapt* @Stringmapset(%mapt*, i8*, i32)

declare void @Listlistset(i8**, i32, i8*)

declare void @Charlistset(i8*, i32, i8)

declare %charmapt* @Charmapdestroy(%charmapt*, i8)

declare %mapt* @Stringmapdestroy(%mapt*, i8*)

declare i8 @Charmapcontains(%charmapt*, i8)

declare i8 @Stringmapcontains(%mapt*, i8*)

declare void @FillListlist(i8**, i32, i32)

declare void @FillList(i8*, i32)

declare void @exit(i32)

declare i8 @ToAscii(i32)

declare i32 @OllehRandom(i32)

declare i8* @readInput()

declare %mapt* @readDict(i8*)

declare void @InitializeRandom()

declare i8* @anagram(%mapt*, i8*)

define void @main() {
entry:
  %lis = alloca i8*
  %lis_2 = alloca i8**
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
  store i8* %a1, i8** @lis
  %lis5 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis5)
  %lis6 = load i8*, i8** @lis
  call void @Charlistset(i8* %lis6, i32 2, i8 53)
  %lis7 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis7)
  %lis8 = load i8*, i8** @lis
  %Charlistget_result = call i8 @Charlistget(i8* %lis8, i32 0)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.2, i32 0, i32 0), i8 %Charlistget_result)
  %a19 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32), i32 3))
  %a310 = getelementptr i8, i8* %a19, i32 3
  store i8 0, i8* %a310
  call void @FillList(i8* %a19, i32 2)
  store i8* %a19, i8** @lis
  %lis11 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis11)
  %a112 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %a313 = getelementptr i8, i8* %a112, i32 0
  store i8 0, i8* %a313
  store i8* %a112, i8** @lis
  %lis14 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis14)
  %a115 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %a316 = getelementptr i8, i8* %a115, i32 0
  store i8 0, i8* %a316
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32), i32 2))
  %a117 = bitcast i8* %malloccall to i8**
  %a318 = getelementptr i8*, i8** %a117, i32 0
  store i8* %a115, i8** %a318
  %a319 = getelementptr i8*, i8** %a117, i32 1
  store i8* null, i8** %a319
  store i8** %a117, i8*** @lis_2
  %lis_220 = load i8**, i8*** @lis_2
  call void @PrintListList(i8** %lis_220)
  %malloccall21 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32), i32 3))
  %a122 = bitcast i8* %malloccall21 to i8**
  %a323 = getelementptr i8*, i8** %a122, i32 3
  store i8* null, i8** %a323
  call void @FillListlist(i8** %a122, i32 2, i32 3)
  store i8** %a122, i8*** @lis_2
  %lis_224 = load i8**, i8*** @lis_2
  call void @Listlistset(i8** %lis_224, i32 0, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @0, i32 0, i32 0))
  %lis_225 = load i8**, i8*** @lis_2
  %Listlistget_result = call i8* @Listlistget(i8** %lis_225, i32 0)
  call void @PrintCharLis(i8* %Listlistget_result)
  ret void
}

declare noalias i8* @malloc(i32)
