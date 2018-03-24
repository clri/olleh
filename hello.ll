; ModuleID = 'Olleh'
source_filename = "Olleh"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@0 = private unnamed_addr constant [16 x i8] c"\22hello, world!\22\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @0, i32 0, i32 0))
  ret void
}
