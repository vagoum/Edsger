; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [14 x i8] c"\0A!dlrow olleH\00"

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
  %p = alloca i8*
  %malloccall = tail call i8* @malloc(i32 mul (i32 trunc (i64 mul nuw (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 20) to i32), i32 20))
  %mallocttmp = bitcast i8* %malloccall to [20 x i8]*
  %tmp = bitcast [20 x i8]* %mallocttmp to i8*
  store i8* %tmp, i8** %p
  %tmp1 = load i8*, i8** %p
  call void @reverse(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @tmp, i32 0, i32 0), i8* %tmp1)
  %tmp2 = load i8*, i8** %p
  call void @writeString(i8* %tmp2)
  ret void
}

define void @reverse(i8* %s, i8* %r) {
entry:
  %s1 = alloca i8*
  store i8* %s, i8** %s1
  %r2 = alloca i8*
  store i8* %r, i8** %r2
  %i = alloca i32
  %l = alloca i32
  store i32 0, i32* %i
  %tmp = load i8*, i8** %s1
  %t = call i32 @strlen(i8* %tmp)
  store i32 %t, i32* %l
  br label %cond

loop:                                             ; preds = %cond
  %tmp3 = load i32, i32* %i
  %tmp4 = load i8*, i8** %r2
  %arrayval = getelementptr i8, i8* %tmp4, i32 %tmp3
  %tmp5 = load i32, i32* %l
  %tmp6 = load i32, i32* %i
  %subtmp = sub i32 %tmp5, %tmp6
  %subtmp7 = sub i32 %subtmp, 1
  %tmp8 = load i8*, i8** %s1
  %accs = getelementptr i8, i8* %tmp8, i32 %subtmp7
  %tmp9 = load i8, i8* %accs
  store i8 %tmp9, i8* %arrayval
  br label %inc

inc:                                              ; preds = %loop
  %tmp10 = load i32, i32* %i
  %addtmp = add i32 %tmp10, 1
  store i32 %addtmp, i32* %i
  %tmp11 = load i32, i32* %i
  %subtmp12 = sub i32 %tmp11, 1
  br label %cond

cond:                                             ; preds = %inc, %entry
  %tmp13 = load i32, i32* %i
  %tmp14 = load i32, i32* %l
  %lttmp = icmp slt i32 %tmp13, %tmp14
  br i1 %lttmp, label %loop, label %after

after:                                            ; preds = %cond
  %tmp15 = load i32, i32* %i
  %tmp16 = load i8*, i8** %r2
  %arrayval17 = getelementptr i8, i8* %tmp16, i32 %tmp15
  store i8 0, i8* %arrayval17
  ret void
}

declare noalias i8* @malloc(i32)
