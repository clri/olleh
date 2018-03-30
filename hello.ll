; ModuleID = 'Olleh'
source_filename = "Olleh"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@0 = private unnamed_addr constant [14 x i8] c"hello, world!\00"

declare i32 @printf(i8*, ...)

declare void @InitializeLocalGarbage()

declare void @CollectLocalGarbage()

define void @main() {
entry:
  call void @InitializeLocalGarbage()
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @0, i32 0, i32 0))
  call void @CollectLocalGarbage()
  ret void
}
