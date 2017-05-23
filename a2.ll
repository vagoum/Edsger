; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [7 x i8] c"Right\0A\00"
@tmp.1 = private unnamed_addr constant [7 x i8] c"Wrong\0A\00"

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

define void @main() {
entry:
  %i = alloca i32
  %n = alloca i32
  store i32 6, i32* %n
  %t = call i32 @factorial()
  store i32 %t, i32* %i
  %tmp = load i32, i32* %i
  %equaltmp = icmp eq i32 720, %tmp
  %ifcond = icmp ne i1 %equaltmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  call void @writeString(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @tmp, i32 0, i32 0))
  br label %ifcond1

else:                                             ; preds = %entry
  call void @writeString(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @tmp.1, i32 0, i32 0))
  br label %ifcond1

ifcond1:                                          ; preds = %else, %then
  ret void
}

define i32 @factorial() {
entry:
  %tmp = load i32, i32* %n
  %equaltmp = icmp eq i32 %tmp, 0
  %ifcond = icmp ne i1 %equaltmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  ret i32 1
  br label %ifcond3

else:                                             ; preds = %entry
  %tmp1 = load i32, i32* %n
  %subtmp = sub i32 %tmp1, 1
  store i32 %subtmp, i32* %n
  %tmp2 = load i32, i32* %n
  %addtmp = add i32 %tmp2, 1
  %t = call i32 @factorial()
  %multmp = mul i32 %addtmp, %t
  ret i32 %multmp
  br label %ifcond3

ifcond3:                                          ; preds = %else, %then
  ret i32 0
}
