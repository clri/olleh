; ModuleID = 'Olleh'
source_filename = "Olleh"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define void @_start() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), [16 x i8] c"\22hello, world!\22\00")
  ret void
}
