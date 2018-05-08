; ModuleID = 'Olleh'
source_filename = "Olleh"

%mapt = type { i8*, i32, %mapt* }
%playert = type { i32, i1, i8*, %mapt* }
%charmapt = type { i8, i32, %charmapt* }

@dictionary = global %mapt* null
@v2 = global i1 false
@v1 = global i1 false
@inp = global i8* null
@p2 = global %playert* null
@p1 = global %playert* null
@letterScores = global %charmapt* null
@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"
@0 = private unnamed_addr constant [22 x i8] c"Player One's Letters:\00"
@1 = private unnamed_addr constant [22 x i8] c"Player Two's Letters:\00"
@2 = private unnamed_addr constant [203 x i8] c"Here's how you play: Each player has to make a word using one of their\0A      letters as the first and last letters of the word. Each word counts for\0A      its length in points. First to ten points wins!\00"
@3 = private unnamed_addr constant [24 x i8] c"It's Player One's turn!\00"
@4 = private unnamed_addr constant [17 x i8] c"input received: \00"
@5 = private unnamed_addr constant [30 x i8] c"Invalid word; switching turns\00"
@6 = private unnamed_addr constant [27 x i8] c"Game over! Player One Wins\00"
@7 = private unnamed_addr constant [36 x i8] c"Nice one! Now your turn, Player Two\00"
@8 = private unnamed_addr constant [27 x i8] c"Game over! Player Two Wins\00"
@9 = private unnamed_addr constant [32 x i8] c"Whoa! Now your turn, Player One\00"
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
@fmt.24 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.25 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.26 = private unnamed_addr constant [6 x i8] c"'%c'\0A\00"

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
  %p1 = alloca %playert*
  %p2 = alloca %playert*
  %inp = alloca i8*
  %v1 = alloca i1
  %v2 = alloca i1
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
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp2 = bitcast i8* %malloccall1 to %playert*
  store %playert { i32 0, i1 true, i8* null, %mapt* null }, %playert* %tmp2
  store %playert* %tmp2, %playert** @p1
  %setLetters_result = call i8* @setLetters(i32 7)
  %p13 = load %playert*, %playert** @p1
  %playerval = load %playert, %playert* %p13
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp5 = bitcast i8* %malloccall4 to %playert*
  %aa = insertvalue %playert %playerval, i8* %setLetters_result, 2
  store %playert %aa, %playert* %tmp5
  store %playert* %tmp5, %playert** @p1
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([22 x i8], [22 x i8]* @0, i32 0, i32 0))
  %p16 = load %playert*, %playert** @p1
  %tmp7 = getelementptr inbounds %playert, %playert* %p16, i32 0, i32 2
  %tmp28 = load i8*, i8** %tmp7
  call void @PrintCharLis(i8* %tmp28)
  %malloccall9 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp10 = bitcast i8* %malloccall9 to %playert*
  store %playert zeroinitializer, %playert* %tmp10
  store %playert* %tmp10, %playert** @p2
  %setLetters_result11 = call i8* @setLetters(i32 7)
  %p212 = load %playert*, %playert** @p2
  %playerval13 = load %playert, %playert* %p212
  %malloccall14 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp15 = bitcast i8* %malloccall14 to %playert*
  %aa16 = insertvalue %playert %playerval13, i8* %setLetters_result11, 2
  store %playert %aa16, %playert* %tmp15
  store %playert* %tmp15, %playert** @p2
  %printf17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([22 x i8], [22 x i8]* @1, i32 0, i32 0))
  %p218 = load %playert*, %playert** @p2
  %tmp19 = getelementptr inbounds %playert, %playert* %p218, i32 0, i32 2
  %tmp220 = load i8*, i8** %tmp19
  call void @PrintCharLis(i8* %tmp220)
  %printf21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([203 x i8], [203 x i8]* @2, i32 0, i32 0))
  %printf22 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @3, i32 0, i32 0))
  br label %while

while:                                            ; preds = %merge111, %entry
  %p1180 = load %playert*, %playert** @p1
  %tmp181 = getelementptr inbounds %playert, %playert* %p1180, i32 0, i32 0
  %tmp2182 = load i32, i32* %tmp181
  %tmp183 = icmp slt i32 %tmp2182, 10
  %p2184 = load %playert*, %playert** @p2
  %tmp185 = getelementptr inbounds %playert, %playert* %p2184, i32 0, i32 0
  %tmp2186 = load i32, i32* %tmp185
  %tmp187 = icmp slt i32 %tmp2186, 10
  %tmp188 = and i1 %tmp183, %tmp187
  br i1 %tmp188, label %while_body, label %merge189

while_body:                                       ; preds = %while
  %rinput_result = call i8* @readInput()
  store i8* %rinput_result, i8** @inp
  %inp23 = load i8*, i8** @inp
  %strcat_result = call i8* @SConcat(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @4, i32 0, i32 0), i8* %inp23)
  %printf24 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* %strcat_result)
  store i1 false, i1* @v1
  store i1 false, i1* @v2
  %p125 = load %playert*, %playert** @p1
  %tmp26 = getelementptr inbounds %playert, %playert* %p125, i32 0, i32 1
  %tmp227 = load i1, i1* %tmp26
  %c1 = alloca i8
  %c2 = alloca i8
  %ll = alloca i8
  %c11 = alloca i8
  %c22 = alloca i8
  %lll = alloca i8
  br i1 %tmp227, label %then, label %else65

merge:                                            ; preds = %merge106, %merge64
  %v1107 = load i1, i1* @v1
  %v2108 = load i1, i1* @v2
  %tmp109 = and i1 %v1107, %v2108
  %tmp110 = xor i1 %tmp109, true
  br i1 %tmp110, label %then112, label %else114

then:                                             ; preds = %while_body
  %inp28 = load i8*, i8** @inp
  %Charlistget_result = call i8 @Charlistget(i8* %inp28, i32 0)
  store i8 %Charlistget_result, i8* %c1
  %inp29 = load i8*, i8** @inp
  %clen_result = call i32 @strlen(i8* %inp29)
  %tmp30 = sub i32 %clen_result, 1
  %inp31 = load i8*, i8** @inp
  %Charlistget_result32 = call i8 @Charlistget(i8* %inp31, i32 %tmp30)
  store i8 %Charlistget_result32, i8* %c2
  %ll33 = alloca i8
  %_ = alloca i32
  store i32 0, i32* %_
  %p134 = load %playert*, %playert** @p1
  %tmp35 = getelementptr inbounds %playert, %playert* %p134, i32 0, i32 2
  %tmp236 = load i8*, i8** %tmp35
  %Charlistget_result37 = call i8 @Charlistget(i8* %tmp236, i32 0)
  store i8 %Charlistget_result37, i8* %ll33
  br label %while38

while38:                                          ; preds = %merge53, %then
  %_58 = load i32, i32* %_
  %p159 = load %playert*, %playert** @p1
  %tmp60 = getelementptr inbounds %playert, %playert* %p159, i32 0, i32 2
  %tmp261 = load i8*, i8** %tmp60
  %clen_result62 = call i32 @strlen(i8* %tmp261)
  %tmp63 = icmp slt i32 %_58, %clen_result62
  br i1 %tmp63, label %while_body39, label %merge64

while_body39:                                     ; preds = %while38
  %_40 = load i32, i32* %_
  %p141 = load %playert*, %playert** @p1
  %tmp42 = getelementptr inbounds %playert, %playert* %p141, i32 0, i32 2
  %tmp243 = load i8*, i8** %tmp42
  %Charlistget_result44 = call i8 @Charlistget(i8* %tmp243, i32 %_40)
  store i8 %Charlistget_result44, i8* %ll33
  %ll45 = load i8, i8* %ll33
  %c146 = load i8, i8* %c1
  %tmp47 = icmp eq i8 %ll45, %c146
  br i1 %tmp47, label %then49, label %else

merge48:                                          ; preds = %else, %then49
  %ll50 = load i8, i8* %ll33
  %c251 = load i8, i8* %c2
  %tmp52 = icmp eq i8 %ll50, %c251
  br i1 %tmp52, label %then54, label %else55

then49:                                           ; preds = %while_body39
  store i1 true, i1* @v1
  br label %merge48

else:                                             ; preds = %while_body39
  br label %merge48

merge53:                                          ; preds = %else55, %then54
  %_56 = load i32, i32* %_
  %tmp57 = add i32 %_56, 1
  store i32 %tmp57, i32* %_
  br label %while38

then54:                                           ; preds = %merge48
  store i1 true, i1* @v2
  br label %merge53

else55:                                           ; preds = %merge48
  br label %merge53

merge64:                                          ; preds = %while38
  br label %merge

else65:                                           ; preds = %while_body
  %inp66 = load i8*, i8** @inp
  %Charlistget_result67 = call i8 @Charlistget(i8* %inp66, i32 0)
  store i8 %Charlistget_result67, i8* %c11
  %inp68 = load i8*, i8** @inp
  %clen_result69 = call i32 @strlen(i8* %inp68)
  %tmp70 = sub i32 %clen_result69, 1
  %inp71 = load i8*, i8** @inp
  %Charlistget_result72 = call i8 @Charlistget(i8* %inp71, i32 %tmp70)
  store i8 %Charlistget_result72, i8* %c22
  %lll73 = alloca i8
  %_74 = alloca i32
  store i32 0, i32* %_74
  %p275 = load %playert*, %playert** @p2
  %tmp76 = getelementptr inbounds %playert, %playert* %p275, i32 0, i32 2
  %tmp277 = load i8*, i8** %tmp76
  %Charlistget_result78 = call i8 @Charlistget(i8* %tmp277, i32 0)
  store i8 %Charlistget_result78, i8* %lll73
  br label %while79

while79:                                          ; preds = %merge95, %else65
  %_100 = load i32, i32* %_74
  %p2101 = load %playert*, %playert** @p2
  %tmp102 = getelementptr inbounds %playert, %playert* %p2101, i32 0, i32 2
  %tmp2103 = load i8*, i8** %tmp102
  %clen_result104 = call i32 @strlen(i8* %tmp2103)
  %tmp105 = icmp slt i32 %_100, %clen_result104
  br i1 %tmp105, label %while_body80, label %merge106

while_body80:                                     ; preds = %while79
  %_81 = load i32, i32* %_74
  %p282 = load %playert*, %playert** @p2
  %tmp83 = getelementptr inbounds %playert, %playert* %p282, i32 0, i32 2
  %tmp284 = load i8*, i8** %tmp83
  %Charlistget_result85 = call i8 @Charlistget(i8* %tmp284, i32 %_81)
  store i8 %Charlistget_result85, i8* %lll73
  %lll86 = load i8, i8* %lll73
  %c1187 = load i8, i8* %c11
  %tmp88 = icmp eq i8 %lll86, %c1187
  br i1 %tmp88, label %then90, label %else91

merge89:                                          ; preds = %else91, %then90
  %lll92 = load i8, i8* %lll73
  %c2293 = load i8, i8* %c22
  %tmp94 = icmp eq i8 %lll92, %c2293
  br i1 %tmp94, label %then96, label %else97

then90:                                           ; preds = %while_body80
  store i1 true, i1* @v1
  br label %merge89

else91:                                           ; preds = %while_body80
  br label %merge89

merge95:                                          ; preds = %else97, %then96
  %_98 = load i32, i32* %_74
  %tmp99 = add i32 %_98, 1
  store i32 %tmp99, i32* %_74
  br label %while79

then96:                                           ; preds = %merge89
  store i1 true, i1* @v2
  br label %merge95

else97:                                           ; preds = %merge89
  br label %merge95

merge106:                                         ; preds = %while79
  br label %merge

merge111:                                         ; preds = %merge118, %then112
  call void @switchTurns()
  br label %while

then112:                                          ; preds = %merge
  %printf113 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([30 x i8], [30 x i8]* @5, i32 0, i32 0))
  br label %merge111

else114:                                          ; preds = %merge
  %p1115 = load %playert*, %playert** @p1
  %tmp116 = getelementptr inbounds %playert, %playert* %p1115, i32 0, i32 1
  %tmp2117 = load i1, i1* %tmp116
  br i1 %tmp2117, label %then119, label %else149

merge118:                                         ; preds = %merge175, %merge144
  br label %merge111

then119:                                          ; preds = %else114
  %inp120 = load i8*, i8** @inp
  %p1121 = load %playert*, %playert** @p1
  %tmp122 = getelementptr inbounds %playert, %playert* %p1121, i32 0, i32 3
  %tmp2123 = load %mapt*, %mapt** %tmp122
  %Smset_result = call %mapt* @Stringmapset(%mapt* %tmp2123, i8* %inp120, i32 1)
  %p1124 = load %playert*, %playert** @p1
  %playerval125 = load %playert, %playert* %p1124
  %malloccall126 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp127 = bitcast i8* %malloccall126 to %playert*
  %aa128 = insertvalue %playert %playerval125, %mapt* %Smset_result, 3
  store %playert %aa128, %playert* %tmp127
  store %playert* %tmp127, %playert** @p1
  %p1129 = load %playert*, %playert** @p1
  %tmp130 = getelementptr inbounds %playert, %playert* %p1129, i32 0, i32 0
  %tmp2131 = load i32, i32* %tmp130
  %inp132 = load i8*, i8** @inp
  %clen_result133 = call i32 @strlen(i8* %inp132)
  %tmp134 = add i32 %tmp2131, %clen_result133
  %p1135 = load %playert*, %playert** @p1
  %playerval136 = load %playert, %playert* %p1135
  %malloccall137 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp138 = bitcast i8* %malloccall137 to %playert*
  %aa139 = insertvalue %playert %playerval136, i32 %tmp134, 0
  store %playert %aa139, %playert* %tmp138
  store %playert* %tmp138, %playert** @p1
  %p1140 = load %playert*, %playert** @p1
  %tmp141 = getelementptr inbounds %playert, %playert* %p1140, i32 0, i32 0
  %tmp2142 = load i32, i32* %tmp141
  %tmp143 = icmp sge i32 %tmp2142, 10
  br i1 %tmp143, label %then145, label %else147

merge144:                                         ; preds = %else147, %then145
  %printf148 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([36 x i8], [36 x i8]* @7, i32 0, i32 0))
  br label %merge118

then145:                                          ; preds = %then119
  %printf146 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([27 x i8], [27 x i8]* @6, i32 0, i32 0))
  call void @exit(i32 0)
  br label %merge144

else147:                                          ; preds = %then119
  br label %merge144

else149:                                          ; preds = %else114
  %inp150 = load i8*, i8** @inp
  %p2151 = load %playert*, %playert** @p2
  %tmp152 = getelementptr inbounds %playert, %playert* %p2151, i32 0, i32 3
  %tmp2153 = load %mapt*, %mapt** %tmp152
  %Smset_result154 = call %mapt* @Stringmapset(%mapt* %tmp2153, i8* %inp150, i32 1)
  %p2155 = load %playert*, %playert** @p2
  %playerval156 = load %playert, %playert* %p2155
  %malloccall157 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp158 = bitcast i8* %malloccall157 to %playert*
  %aa159 = insertvalue %playert %playerval156, %mapt* %Smset_result154, 3
  store %playert %aa159, %playert* %tmp158
  store %playert* %tmp158, %playert** @p2
  %p1160 = load %playert*, %playert** @p1
  %tmp161 = getelementptr inbounds %playert, %playert* %p1160, i32 0, i32 0
  %tmp2162 = load i32, i32* %tmp161
  %inp163 = load i8*, i8** @inp
  %clen_result164 = call i32 @strlen(i8* %inp163)
  %tmp165 = add i32 %tmp2162, %clen_result164
  %p2166 = load %playert*, %playert** @p2
  %playerval167 = load %playert, %playert* %p2166
  %malloccall168 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp169 = bitcast i8* %malloccall168 to %playert*
  %aa170 = insertvalue %playert %playerval167, i32 %tmp165, 0
  store %playert %aa170, %playert* %tmp169
  store %playert* %tmp169, %playert** @p2
  %p2171 = load %playert*, %playert** @p2
  %tmp172 = getelementptr inbounds %playert, %playert* %p2171, i32 0, i32 0
  %tmp2173 = load i32, i32* %tmp172
  %tmp174 = icmp sge i32 %tmp2173, 10
  br i1 %tmp174, label %then176, label %else178

merge175:                                         ; preds = %else178, %then176
  %printf179 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([32 x i8], [32 x i8]* @9, i32 0, i32 0))
  br label %merge118

then176:                                          ; preds = %else149
  %printf177 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([27 x i8], [27 x i8]* @8, i32 0, i32 0))
  call void @exit(i32 0)
  br label %merge175

else178:                                          ; preds = %else149
  br label %merge175

merge189:                                         ; preds = %while
  ret void
}

define void @switchTurns() {
entry:
  %p1 = load %playert*, %playert** @p1
  %tmp = getelementptr inbounds %playert, %playert* %p1, i32 0, i32 1
  %tmp2 = load i1, i1* %tmp
  br i1 %tmp2, label %then, label %else

merge:                                            ; preds = %else, %then
  ret void

then:                                             ; preds = %entry
  %p2 = load %playert*, %playert** @p2
  %playerval = load %playert, %playert* %p2
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp1 = bitcast i8* %malloccall to %playert*
  %aa = insertvalue %playert %playerval, i1 true, 1
  store %playert %aa, %playert* %tmp1
  store %playert* %tmp1, %playert** @p2
  %p12 = load %playert*, %playert** @p1
  %playerval3 = load %playert, %playert* %p12
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp5 = bitcast i8* %malloccall4 to %playert*
  %aa6 = insertvalue %playert %playerval3, i1 false, 1
  store %playert %aa6, %playert* %tmp5
  store %playert* %tmp5, %playert** @p1
  br label %merge

else:                                             ; preds = %entry
  %p17 = load %playert*, %playert** @p1
  %playerval8 = load %playert, %playert* %p17
  %malloccall9 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp10 = bitcast i8* %malloccall9 to %playert*
  %aa11 = insertvalue %playert %playerval8, i1 true, 1
  store %playert %aa11, %playert* %tmp10
  store %playert* %tmp10, %playert** @p1
  %p212 = load %playert*, %playert** @p2
  %playerval13 = load %playert, %playert* %p212
  %malloccall14 = tail call i8* @malloc(i32 ptrtoint (%playert* getelementptr (%playert, %playert* null, i32 1) to i32))
  %tmp15 = bitcast i8* %malloccall14 to %playert*
  %aa16 = insertvalue %playert %playerval13, i1 false, 1
  store %playert %aa16, %playert* %tmp15
  store %playert* %tmp15, %playert** @p2
  br label %merge
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

define i8* @setLetters(i32 %len_) {
entry:
  %len_1 = alloca i32
  store i32 %len_, i32* %len_1
  %temp_ = alloca i8*
  %i_ = alloca i32
  %ch_ = alloca i32
  %len_2 = load i32, i32* %len_1
  %tmp = add i32 %len_2, 1
  %mallocsize = mul i32 %tmp, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %a1 = tail call i8* @malloc(i32 %mallocsize)
  %a3 = getelementptr i8, i8* %a1, i32 %tmp
  store i8 0, i8* %a3
  call void @FillList(i8* %a1, i32 %len_2)
  store i8* %a1, i8** %temp_
  store i32 0, i32* %i_
  br label %while

while:                                            ; preds = %while_body, %entry
  %i_10 = load i32, i32* %i_
  %len_11 = load i32, i32* %len_1
  %tmp12 = icmp slt i32 %i_10, %len_11
  br i1 %tmp12, label %while_body, label %merge

while_body:                                       ; preds = %while
  %rand_result = call i32 @OllehRandom(i32 26)
  %tmp3 = add i32 %rand_result, 97
  store i32 %tmp3, i32* %ch_
  %ch_4 = load i32, i32* %ch_
  %tmp5 = call i8 @ToAscii(i32 %ch_4)
  %i_6 = load i32, i32* %i_
  %temp_7 = load i8*, i8** %temp_
  call void @Charlistset(i8* %temp_7, i32 %i_6, i8 %tmp5)
  %i_8 = load i32, i32* %i_
  %tmp9 = add i32 %i_8, 1
  store i32 %tmp9, i32* %i_
  br label %while

merge:                                            ; preds = %while
  %temp_13 = load i8*, i8** %temp_
  ret i8* %temp_13
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
  %lett_2 = alloca i8
  %_ = alloca i32
  store i32 0, i32* %_
  %letters_3 = load %charmapt*, %charmapt** %letters_1
  %Charmapgeti_result = call i8 @Charmapgeti(%charmapt* %letters_3, i32 0)
  store i8 %Charmapgeti_result, i8* %lett_2
  br label %while

while:                                            ; preds = %while_body, %entry
  %_11 = load i32, i32* %_
  %letters_12 = load %charmapt*, %charmapt** %letters_1
  %cmlen_result = call i32 @CharmapgetLength(%charmapt* %letters_12)
  %tmp13 = icmp slt i32 %_11, %cmlen_result
  br i1 %tmp13, label %while_body, label %merge

while_body:                                       ; preds = %while
  %_4 = load i32, i32* %_
  %letters_5 = load %charmapt*, %charmapt** %letters_1
  %Charmapgeti_result6 = call i8 @Charmapgeti(%charmapt* %letters_5, i32 %_4)
  store i8 %Charmapgeti_result6, i8* %lett_2
  %lett_7 = load i8, i8* %lett_2
  %letters_8 = load %charmapt*, %charmapt** %letters_1
  %Charmapget_result = call i32 @Charmapget(%charmapt* %letters_8, i8 %lett_7)
  %lett_9 = load i8, i8* %lett_2
  %letterScores = load %charmapt*, %charmapt** @letterScores
  %Cmset_result = call %charmapt* @Charmapset(%charmapt* %letterScores, i8 %lett_9, i32 %Charmapget_result)
  store %charmapt* %Cmset_result, %charmapt** @letterScores
  %_10 = load i32, i32* %_
  %tmp = add i32 %_10, 1
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
