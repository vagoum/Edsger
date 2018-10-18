; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [12 x i8] c"Hello World\00"

declare void @writeString(i8*)

declare void @writeInteger(i32)

declare void @writeBoolean(i1)

declare void @writeChar(i8)

declare void @writeReal(x86_fp80)

declare i32 @readInteger()

declare i1 @readBoolean()

declare i8 @readChar()

declare x86_fp80 @readReal()

declare x86_fp80 @readString(i32, i8*)

declare i32 @abs(i32)

declare x86_fp80 @fabs(x86_fp80)

declare x86_fp80 @sqrt(x86_fp80)

declare x86_fp80 @sin(x86_fp80)

declare x86_fp80 @cos(x86_fp80)

declare x86_fp80 @tan(x86_fp80)

declare x86_fp80 @atan(x86_fp80)

declare x86_fp80 @exp(x86_fp80)

declare x86_fp80 @ln(x86_fp80)

declare x86_fp80 @pi()

declare i32 @trunc(x86_fp80)

declare i32 @round(x86_fp80)

declare i32 @ord(i8)

declare i8 @chr(i32)

declare i32 @strlen(i8*)

declare i32 @strcmp(i8*, i8*)

declare void @strcpy(i8*, i8*)

declare void @strcat(i8*, i8*)

define i32 @main() {
entry:
  call void @writeString(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}
