; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [5 x i8] c"'\5C0'\00"
@tmp.1 = private unnamed_addr constant [17 x i8] c"\22\5Cn!dlrow olleH\22\00"

declare void @writeString(i8*)

declare void @writeInteger(i32)

declare void @writeBoolean(i1)

declare void @writeChar(i8)

declare void @writeReal(double)

declare i32 @readInteger()

declare i1 @readBoolean()

declare i8 @readChar()

declare double @readReal()

declare double @readString(i32, i8*)

declare i32 @abs(i32)

declare double @fabs(double)

declare double @sqrt(double)

declare double @sin(double)

declare double @cos(double)

declare double @tan(double)

declare double @atan(double)

declare double @exp(double)

declare double @ln(double)

declare double @pi()

declare i32 @trunc(double)

declare i32 @round(double)

declare i32 @ord(i8)

declare i8 @chr(i32)

declare i32 @strlen(i8*)

declare i32 @strcmp(i8*, i8*)

declare void @strcpy(i8*, i8*)

declare void @strcat(i8*, i8*)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %p = bitcast i8* %malloccall to i8**
  call void @reverse(i8** %p)
  ret i32 0
}

define void @reverse(i8* %s, i8* %r) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %i = bitcast i8* %malloccall to i32*
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %l = bitcast i8* %malloccall1 to i32*
  %0 = call i32 @strlen(i8* %s)
  store i32 %0, i32* %l
  store i32 %0, i32* %i
  br label %cond

loop:                                             ; preds = %cond
  %subtmp = fsub i32* %l, %i
  %subtmp2 = fsub i32* %subtmp, i32 1
  %array = getelementptr i8, i8* %s, i32* %subtmp2
  %array3 = load i8, i8* %array
  %arrayval = getelementptr i8, i8* %r, i32* %i
  store i8 %array3, i8* %arrayval
  br label %inc

inc:                                              ; preds = %loop
  %addtmp = fadd i32* %i, i32 1
  store i32* %addtmp, i32* %i
  %subtmp4 = fsub i32* %i, i32 1
  br label %cond

cond:                                             ; preds = %inc, %entry
  %lttmp = fcmp ult i32* %i, %l
  br i1 %lttmp, label %loop, label %after
  %arrayval5 = getelementptr i8, i8* %r, i32* %i
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @tmp, i32 0, i32 0), i8* %arrayval5
  ret i32 0

after:                                            ; preds = %cond
}

declare noalias i8* @malloc(i32)
