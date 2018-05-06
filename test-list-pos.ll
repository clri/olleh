; ModuleID = 'Olleh'
source_filename = "Olleh"

%mapt = type { i8*, i32, %mapt* }
%charmapt = type { i8, i32, %charmapt* }
%playert = type { i32, i1, i8*, %mapt* }

@dictionary = global %mapt* null
@lis_2 = global i8** null
@lis = global i8* null
@letterScores = global %charmapt* null
@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@0 = private unnamed_addr constant [11 x i8] c"zero list:\00"
@1 = private unnamed_addr constant [4 x i8] c"abc\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.5 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.7 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.8 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@fmt.9 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.11 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@fmt.12 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.13 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.14 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@fmt.15 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.16 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.17 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@fmt.18 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.19 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.20 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@fmt.21 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.22 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.23 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"

declare i32 @printf(i8*, ...)

declare i8* @intToString(i32)

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
  %letterScores = alloca %charmapt*
  %lis = alloca i8*
  %lis_2 = alloca i8**
  call void @InitializeRandom()
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%charmapt* getelementptr (%charmapt, %charmapt* null, i32 1) to i32))
  %tmp = bitcast i8* %malloccall to %charmapt*
  store %charmapt { i8 90, i32 1, %charmapt* null }, %charmapt* %tmp
  %0 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 89, i32 1)
  %1 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 88, i32 1)
  %2 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 87, i32 1)
  %3 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 86, i32 1)
  %4 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 85, i32 1)
  %5 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 84, i32 1)
  %6 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 83, i32 1)
  %7 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 82, i32 1)
  %8 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 81, i32 1)
  %9 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 80, i32 1)
  %10 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 79, i32 1)
  %11 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 78, i32 1)
  %12 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 77, i32 1)
  %13 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 76, i32 1)
  %14 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 75, i32 1)
  %15 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 74, i32 1)
  %16 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 73, i32 1)
  %17 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 72, i32 1)
  %18 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 71, i32 1)
  %19 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 70, i32 1)
  %20 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 69, i32 1)
  %21 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 68, i32 1)
  %22 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 67, i32 1)
  %23 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 66, i32 1)
  %24 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 65, i32 1)
  %25 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 122, i32 1)
  %26 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 121, i32 1)
  %27 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 120, i32 1)
  %28 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 119, i32 1)
  %29 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 118, i32 1)
  %30 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 117, i32 1)
  %31 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 116, i32 1)
  %32 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 115, i32 1)
  %33 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 114, i32 1)
  %34 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 113, i32 1)
  %35 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 112, i32 1)
  %36 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 111, i32 1)
  %37 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 110, i32 1)
  %38 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 109, i32 1)
  %39 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 108, i32 1)
  %40 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 107, i32 1)
  %41 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 106, i32 1)
  %42 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 105, i32 1)
  %43 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 104, i32 1)
  %44 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 103, i32 1)
  %45 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 102, i32 1)
  %46 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 101, i32 1)
  %47 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 100, i32 1)
  %48 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 99, i32 1)
  %49 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 98, i32 1)
  %50 = call %charmapt* @Charmapset(%charmapt* %tmp, i8 97, i32 1)
  store %charmapt* %tmp, %charmapt** @letterScores
  %a1 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32), i32 5))
  %a3 = getelementptr i8, i8* %a1, i32 0
  store i8 97, i8* %a3
  %a32 = getelementptr i8, i8* %a1, i32 1
  store i8 98, i8* %a32
  %a33 = getelementptr i8, i8* %a1, i32 2
  store i8 99, i8* %a33
  %a34 = getelementptr i8, i8* %a1, i32 3
  store i8 100, i8* %a34
  %a35 = getelementptr i8, i8* %a1, i32 4
  store i8 0, i8* %a35
  store i8* %a1, i8** @lis
  %lis6 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis6)
  %lis7 = load i8*, i8** @lis
  call void @Charlistset(i8* %lis7, i32 2, i8 53)
  %lis8 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis8)
  %lis9 = load i8*, i8** @lis
  %Charlistget_result = call i8 @Charlistget(i8* %lis9, i32 0)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.2, i32 0, i32 0), i8 %Charlistget_result)
  %a111 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32), i32 3))
  %a312 = getelementptr i8, i8* %a111, i32 3
  store i8 0, i8* %a312
  call void @FillList(i8* %a111, i32 2)
  store i8* %a111, i8** @lis
  %lis13 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis13)
  %a115 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %a316 = getelementptr i8, i8* %a115, i32 0
  store i8 0, i8* %a316
  store i8* %a115, i8** @lis
  %lis17 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis17)
  %a119 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %a320 = getelementptr i8, i8* %a119, i32 1
  store i8 0, i8* %a320
  call void @FillList(i8* %a119, i32 0)
  store i8* %a119, i8** @lis
  %printf21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* @0, i32 0, i32 0))
  %lis22 = load i8*, i8** @lis
  call void @PrintCharLis(i8* %lis22)
  %a124 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %a325 = getelementptr i8, i8* %a124, i32 0
  store i8 0, i8* %a325
  %malloccall26 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32), i32 2))
  %a127 = bitcast i8* %malloccall26 to i8**
  %a328 = getelementptr i8*, i8** %a127, i32 0
  store i8* %a124, i8** %a328
  %a329 = getelementptr i8*, i8** %a127, i32 1
  store i8* null, i8** %a329
  store i8** %a127, i8*** @lis_2
  %lis_230 = load i8**, i8*** @lis_2
  call void @PrintListList(i8** %lis_230)
  %malloccall31 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32), i32 3))
  %a132 = bitcast i8* %malloccall31 to i8**
  %a333 = getelementptr i8*, i8** %a132, i32 3
  store i8* null, i8** %a333
  call void @FillListlist(i8** %a132, i32 2, i32 3)
  store i8** %a132, i8*** @lis_2
  %lis_234 = load i8**, i8*** @lis_2
  call void @Listlistset(i8** %lis_234, i32 0, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @1, i32 0, i32 0))
  %lis_235 = load i8**, i8*** @lis_2
  %Listlistget_result = call i8* @Listlistget(i8** %lis_235, i32 0)
  call void @PrintCharLis(i8* %Listlistget_result)
  ret void
}

define i8* @scramble(i8* %w_) {
entry:
  %w_1 = alloca i8*
  store i8* %w_, i8** %w_1
  %i_ = alloca i32
  %len_ = alloca i32
  %x_ = alloca i32
  %temp_ = alloca i8*
  %temp2_ = alloca i8*
  %temp3_ = alloca i8*
  store i32 0, i32* %i_
  %w_2 = load i8*, i8** %w_1
  %getLength_result = call i32 @getLength(i8* %w_2)
  store i32 %getLength_result, i32* %len_
  %len_3 = load i32, i32* %len_
  %tmp = add i32 %len_3, 1
  %mallocsize = mul i32 %tmp, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %a1 = tail call i8* @malloc(i32 %mallocsize)
  %a3 = getelementptr i8, i8* %a1, i32 %tmp
  store i8 0, i8* %a3
  call void @FillList(i8* %a1, i32 %len_3)
  store i8* %a1, i8** %temp_
  %w_4 = load i8*, i8** %w_1
  store i8* %w_4, i8** %temp2_
  %len_5 = load i32, i32* %len_
  %tmp6 = add i32 %len_5, 1
  %mallocsize7 = mul i32 %tmp6, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %a18 = tail call i8* @malloc(i32 %mallocsize7)
  %a39 = getelementptr i8, i8* %a18, i32 %tmp6
  store i8 0, i8* %a39
  call void @FillList(i8* %a18, i32 %len_5)
  store i8* %a18, i8** %temp3_
  br label %while

while:                                            ; preds = %merge, %entry
  %i_27 = load i32, i32* %i_
  %len_28 = load i32, i32* %len_
  %tmp29 = icmp slt i32 %i_27, %len_28
  br i1 %tmp29, label %while_body, label %merge30

while_body:                                       ; preds = %while
  %len_10 = load i32, i32* %len_
  %rand_result = call i32 @OllehRandom(i32 %len_10)
  store i32 %rand_result, i32* %x_
  br label %while11

while11:                                          ; preds = %while_body12, %while_body
  %x_15 = load i32, i32* %x_
  %temp3_16 = load i8*, i8** %temp3_
  %Charlistget_result = call i8 @Charlistget(i8* %temp3_16, i32 %x_15)
  %tmp17 = icmp eq i8 %Charlistget_result, 49
  br i1 %tmp17, label %while_body12, label %merge

while_body12:                                     ; preds = %while11
  %len_13 = load i32, i32* %len_
  %rand_result14 = call i32 @OllehRandom(i32 %len_13)
  store i32 %rand_result14, i32* %x_
  br label %while11

merge:                                            ; preds = %while11
  %i_18 = load i32, i32* %i_
  %temp2_19 = load i8*, i8** %temp2_
  %Charlistget_result20 = call i8 @Charlistget(i8* %temp2_19, i32 %i_18)
  %x_21 = load i32, i32* %x_
  %temp_22 = load i8*, i8** %temp_
  call void @Charlistset(i8* %temp_22, i32 %x_21, i8 %Charlistget_result20)
  %x_23 = load i32, i32* %x_
  %temp3_24 = load i8*, i8** %temp3_
  call void @Charlistset(i8* %temp3_24, i32 %x_23, i8 49)
  %i_25 = load i32, i32* %i_
  %tmp26 = add i32 %i_25, 1
  store i32 %tmp26, i32* %i_
  br label %while

merge30:                                          ; preds = %while
  %temp_31 = load i8*, i8** %temp_
  ret i8* %temp_31
}

define i8* @reverse(i8* %w_) {
entry:
  %w_1 = alloca i8*
  store i8* %w_, i8** %w_1
  %i_ = alloca i32
  %len_ = alloca i32
  %temp_ = alloca i8*
  %temp2_ = alloca i8*
  store i32 0, i32* %i_
  %w_2 = load i8*, i8** %w_1
  %getLength_result = call i32 @getLength(i8* %w_2)
  store i32 %getLength_result, i32* %len_
  %len_3 = load i32, i32* %len_
  %tmp = add i32 %len_3, 1
  %mallocsize = mul i32 %tmp, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %a1 = tail call i8* @malloc(i32 %mallocsize)
  %a3 = getelementptr i8, i8* %a1, i32 %tmp
  store i8 0, i8* %a3
  call void @FillList(i8* %a1, i32 %len_3)
  store i8* %a1, i8** %temp_
  %w_4 = load i8*, i8** %w_1
  store i8* %w_4, i8** %temp2_
  br label %while

while:                                            ; preds = %while_body, %entry
  %i_14 = load i32, i32* %i_
  %len_15 = load i32, i32* %len_
  %tmp16 = icmp slt i32 %i_14, %len_15
  br i1 %tmp16, label %while_body, label %merge

while_body:                                       ; preds = %while
  %len_5 = load i32, i32* %len_
  %i_6 = load i32, i32* %i_
  %tmp7 = sub i32 %len_5, %i_6
  %tmp8 = sub i32 %tmp7, 1
  %temp2_9 = load i8*, i8** %temp2_
  %Charlistget_result = call i8 @Charlistget(i8* %temp2_9, i32 %tmp8)
  %i_10 = load i32, i32* %i_
  %temp_11 = load i8*, i8** %temp_
  call void @Charlistset(i8* %temp_11, i32 %i_10, i8 %Charlistget_result)
  %i_12 = load i32, i32* %i_
  %tmp13 = add i32 %i_12, 1
  store i32 %tmp13, i32* %i_
  br label %while

merge:                                            ; preds = %while
  %temp_17 = load i8*, i8** %temp_
  ret i8* %temp_17
}

define i32 @getLength(i8* %w_) {
entry:
  %w_1 = alloca i8*
  store i8* %w_, i8** %w_1
  %temp_ = alloca i8*
  %w_2 = load i8*, i8** %w_1
  store i8* %w_2, i8** %temp_
  %temp_3 = load i8*, i8** %temp_
  %clen_result = call i32 @strlen(i8* %temp_3)
  ret i32 %clen_result
}

define void @setLetters(%playert* %player_) {
entry:
  %player_1 = alloca %playert*
  store %playert* %player_, %playert** %player_1
  %len_ = alloca i32
  %temp_ = alloca i8*
  %i_ = alloca i32
  %ch_ = alloca i32
  %player_2 = load %playert*, %playert** %player_1
  %tmp = getelementptr inbounds %playert, %playert* %player_2, i32 0, i32 2
  %tmp2 = load i8*, i8** %tmp
  %clen_result = call i32 @strlen(i8* %tmp2)
  store i32 %clen_result, i32* %len_
  %len_3 = load i32, i32* %len_
  %tmp4 = add i32 %len_3, 1
  %mallocsize = mul i32 %tmp4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %a1 = tail call i8* @malloc(i32 %mallocsize)
  %a3 = getelementptr i8, i8* %a1, i32 %tmp4
  store i8 0, i8* %a3
  call void @FillList(i8* %a1, i32 %len_3)
  store i8* %a1, i8** %temp_
  store i32 0, i32* %i_
  br label %while

while:                                            ; preds = %while_body, %entry
  %i_12 = load i32, i32* %i_
  %len_13 = load i32, i32* %len_
  %tmp14 = icmp slt i32 %i_12, %len_13
  br i1 %tmp14, label %while_body, label %merge

while_body:                                       ; preds = %while
  %rand_result = call i32 @OllehRandom(i32 26)
  %tmp5 = add i32 %rand_result, 97
  store i32 %tmp5, i32* %ch_
  %ch_6 = load i32, i32* %ch_
  %tmp7 = call i8 @ToAscii(i32 %ch_6)
  %i_8 = load i32, i32* %i_
  %temp_9 = load i8*, i8** %temp_
  call void @Charlistset(i8* %temp_9, i32 %i_8, i8 %tmp7)
  %i_10 = load i32, i32* %i_
  %tmp11 = add i32 %i_10, 1
  store i32 %tmp11, i32* %i_
  br label %while

merge:                                            ; preds = %while
  %temp_15 = load i8*, i8** %temp_
  %player_16 = load %playert*, %playert** %player_1
  %playerval = load %playert, %playert* %player_16
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp17 = bitcast i8* %malloccall to %playert*
  %aa = insertvalue %playert %playerval, i8* %temp_15, 2
  store %playert %aa, %playert* %tmp17
  store %playert* %tmp17, %playert** %player_1
  ret void
}

define i1 @checkWord(i8* %w_) {
entry:
  %w_1 = alloca i8*
  store i8* %w_, i8** %w_1
  %w_2 = load i8*, i8** %w_1
  %dictionary = load %mapt*, %mapt** @dictionary
  %smcontains_result = call i8 @Stringmapcontains(%mapt* %dictionary, i8* %w_2)
  %tmp = icmp eq i8 %smcontains_result, 1
  ret i1 %tmp
}

define void @setLetterScore(%charmapt* %letters_) {
entry:
  %letters_1 = alloca %charmapt*
  store %charmapt* %letters_, %charmapt** %letters_1
  %lett_ = alloca i8
  %_ = alloca i32
  store i32 0, i32* %_
  %letters_2 = load %charmapt*, %charmapt** %letters_1
  %Charmapgeti_result = call i8 @Charmapgeti(%charmapt* %letters_2, i32 0)
  store i8 %Charmapgeti_result, i8* %lett_
  br label %while

while:                                            ; preds = %while_body, %entry
  %_10 = load i32, i32* %_
  %letters_11 = load %charmapt*, %charmapt** %letters_1
  %cmlen_result = call i32 @CharmapgetLength(%charmapt* %letters_11)
  %tmp12 = icmp slt i32 %_10, %cmlen_result
  br i1 %tmp12, label %while_body, label %merge

while_body:                                       ; preds = %while
  %_3 = load i32, i32* %_
  %letters_4 = load %charmapt*, %charmapt** %letters_1
  %Charmapgeti_result5 = call i8 @Charmapgeti(%charmapt* %letters_4, i32 %_3)
  store i8 %Charmapgeti_result5, i8* %lett_
  %lett_6 = load i8, i8* %lett_
  %letters_7 = load %charmapt*, %charmapt** %letters_1
  %Charmapget_result = call i32 @Charmapget(%charmapt* %letters_7, i8 %lett_6)
  %lett_8 = load i8, i8* %lett_
  %letterScores = load %charmapt*, %charmapt** @letterScores
  %Cmset_result = call %charmapt* @Charmapset(%charmapt* %letterScores, i8 %lett_8, i32 %Charmapget_result)
  store %charmapt* %Cmset_result, %charmapt** @letterScores
  %_9 = load i32, i32* %_
  %tmp = add i32 %_9, 1
  store i32 %tmp, i32* %_
  br label %while

merge:                                            ; preds = %while
  ret void
}

define i32 @getWordScore(i8* %w_) {
entry:
  %w_1 = alloca i8*
  store i8* %w_, i8** %w_1
  %w_2 = load i8*, i8** %w_1
  %dictionary = load %mapt*, %mapt** @dictionary
  %Stringmapget_result = call i32 @Stringmapget(%mapt* %dictionary, i8* %w_2)
  ret i32 %Stringmapget_result
}

declare noalias i8* @malloc(i32)
