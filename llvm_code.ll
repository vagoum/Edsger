; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [9 x i8] c"Give n: \00"
@tmp.1 = private unnamed_addr constant [9 x i8] c"Give k: \00"
@tmp.2 = private unnamed_addr constant [7 x i8] c"Mean: \00"
@tmp.3 = private unnamed_addr constant [2 x i8] c"\0A\00"

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
smth:
  %n = alloca i32
  %k = alloca i32
  %i = alloca i32
  %seed = alloca i32
  %sum = alloca x86_fp80
  call void @writeString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @tmp, i32 0, i32 0))
  %t = call i32 @readInteger()
  store i32 %t, i32* %n
  call void @writeString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @tmp.1, i32 0, i32 0))
  %t1 = call i32 @readInteger()
  store i32 %t1, i32* %k
  store i32 0, i32* %i
  store x86_fp80 0xK00000000000000000000, x86_fp80* %sum
  store i32 65, i32* %seed
  br label %cond

loop:                                             ; preds = %cond
  %tmp = load i32, i32* %seed
  %multmp = mul i32 %tmp, 137
  %addtmp = add i32 %multmp, 221
  %tmp2 = load i32, i32* %i
  %addtmp3 = add i32 %addtmp, %tmp2
  %tmp4 = load i32, i32* %n
  %sremtmp = srem i32 %addtmp3, %tmp4
  store i32 %sremtmp, i32* %seed
  %cast = sitofp i32 %sremtmp to x86_fp80
  %tmp5 = load x86_fp80, x86_fp80* %sum
  %addtmp6 = fadd x86_fp80 %tmp5, %cast
  store x86_fp80 %addtmp6, x86_fp80* %sum
  br label %inc

inc:                                              ; preds = %loop
  %tmp7 = load i32, i32* %i
  %addtmp8 = add i32 %tmp7, 1
  store i32 %addtmp8, i32* %i
  %tmp9 = load i32, i32* %i
  %subtmp = sub i32 %tmp9, 1
  br label %cond

cond:                                             ; preds = %inc, %smth
  %tmp10 = load i32, i32* %i
  %tmp11 = load i32, i32* %k
  %lttmp = icmp slt i32 %tmp10, %tmp11
  br i1 %lttmp, label %loop, label %after

after:                                            ; preds = %cond
  %tmp12 = load i32, i32* %k
  %gttmp = icmp sgt i32 %tmp12, 0
  %ifcond = icmp ne i1 %gttmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %after
  call void @writeString(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @tmp.2, i32 0, i32 0))
  %tmp13 = load i32, i32* %k
  %cast14 = sitofp i32 %tmp13 to x86_fp80
  %tmp15 = load x86_fp80, x86_fp80* %sum
  %divtmp = fdiv x86_fp80 %tmp15, %cast14
  call void @writeReal(x86_fp80 %divtmp)
  call void @writeString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tmp.3, i32 0, i32 0))
  br label %ifcond16

else:                                             ; preds = %after
  br label %ifcond16

ifcond16:                                         ; preds = %else, %then
  ret void
}
