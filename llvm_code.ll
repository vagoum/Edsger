; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [19 x i8] c"cantThinkOfAName: \00"
@tmp.1 = private unnamed_addr constant [8 x i8] c"integ: \00"
@tmp.2 = private unnamed_addr constant [28 x i8] c"x before nestedFirstLevel: \00"
@tmp.3 = private unnamed_addr constant [43 x i8] c"x after nestedFirstLevel (should be 157): \00"
@tmp.4 = private unnamed_addr constant [54 x i8] c"somethingElse after nestedFirstLevel (should be 12): \00"
@tmp.5 = private unnamed_addr constant [5 x i8] c"arr[\00"
@tmp.6 = private unnamed_addr constant [5 x i8] c"] = \00"
@tmp.7 = private unnamed_addr constant [3 x i8] c"x[\00"
@tmp.8 = private unnamed_addr constant [5 x i8] c"] = \00"

declare i32 @trunc(x86_fp80)

declare i32 @round(x86_fp80)

declare i32 @ord(i8)

declare i8 @chr(i32)

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

declare void @writeInteger(i32)

declare void @writeBoolean(i1)

declare void @writeChar(i8)

declare void @writeReal(x86_fp80)

declare void @writeString(i8*)

declare i32 @readInteger()

declare i1 @readBoolean()

declare i8 @readChar()

declare x86_fp80 @readReal()

declare void @readString(i32, i8*)

define i32 @Initialize(i32* %x) {
smth:
  store i32 42, i32* %x
  %tmp = load i32, i32* %x
  %addtmp = add i32 %tmp, 123
  ret i32 %addtmp
}

define void @SomeFunc(i32 %a) {
smth:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %x = alloca i32
  %somethingElse = alloca i32
  store i32 100, i32* %x
  store i32 17, i32* %somethingElse
  call void @writeString(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @tmp.2, i32 0, i32 0))
  %tmp = load i32, i32* %x
  call void @writeInteger(i32 %tmp)
  call void @writeChar(i8 10)
  call void @NestedFirstLevel_SomeFunc(i32 17, i32* %a1, i32* %x)
  call void @writeString(i8* getelementptr inbounds ([43 x i8], [43 x i8]* @tmp.3, i32 0, i32 0))
  %tmp2 = load i32, i32* %x
  call void @writeInteger(i32 %tmp2)
  call void @writeChar(i8 10)
  call void @writeString(i8* getelementptr inbounds ([54 x i8], [54 x i8]* @tmp.4, i32 0, i32 0))
  %tmp3 = load i32, i32* %somethingElse
  call void @writeInteger(i32 %tmp3)
  call void @writeChar(i8 10)
  ret void
}

define i32 @ForwardDeclared_SomeFunc(i32 %b, i32* %a, i32* %somethingElse, i32* %x) {
smth:
  %b1 = alloca i32
  store i32 %b, i32* %b1
  %tmp = load i32, i32* %b1
  %gttmp = icmp sgt i32 %tmp, 0
  %ifcond = icmp ne i1 %gttmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %smth
  %tmp2 = load i32, i32* %b1
  %subtmp = sub i32 %tmp2, 1
  store i32 %subtmp, i32* %b1
  %t = call i32 @ForwardDeclared_SomeFunc(i32 %subtmp, i32* %a, i32* %somethingElse, i32* %x)
  %addtmp = add i32 1, %t
  ret i32 %addtmp
  br label %ifcond3

else:                                             ; preds = %smth
  br label %ifcond3

ifcond3:                                          ; preds = %else, %then
  store i32 12, i32* %somethingElse
  ret i32 42
}

define void @NestedFirstLevel_SomeFunc(i32 %a, i32* %somethingElse, i32* %x) {
smth:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %integ = alloca i32
  %tmp = load i32, i32* %a1
  store i32 %tmp, i32* %integ
  call void @writeString(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @tmp.1, i32 0, i32 0))
  %tmp2 = load i32, i32* %integ
  call void @writeInteger(i32 %tmp2)
  call void @writeChar(i8 10)
  %t = call i32 @NestedSecondLevel_NestedFirstLevel_SomeFunc(i32* %a1, i32* %integ, i32* %somethingElse, i32* %x)
  %tmp3 = load i32, i32* %integ
  %addtmp = add i32 %tmp3, %t
  store i32 %addtmp, i32* %integ
  %tmp4 = load i32, i32* %integ
  store i32 %tmp4, i32* %x
  ret void
}

define i32 @NestedSecondLevel_NestedFirstLevel_SomeFunc(i32* %a, i32* %integ, i32* %somethingElse, i32* %x) {
smth:
  %cantThinkOfAName = alloca i32
  %tmp = load i32, i32* %x
  %t = call i32 @ForwardDeclared_SomeFunc(i32 %tmp, i32* %a, i32* %somethingElse, i32* %x)
  store i32 %t, i32* %cantThinkOfAName
  call void @writeString(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @tmp, i32 0, i32 0))
  %tmp1 = load i32, i32* %cantThinkOfAName
  call void @writeInteger(i32 %tmp1)
  call void @writeChar(i8 10)
  %tmp2 = load i32, i32* %cantThinkOfAName
  %subtmp = sub i32 %tmp2, 102
  %tmp3 = load i32, i32* %x
  %addtmp = add i32 %subtmp, %tmp3
  ret i32 %addtmp
}

define void @FillAndPrint(i32* %arr, i32 %nElements) {
smth:
  %arr1 = alloca i32*
  store i32* %arr, i32** %arr1
  %nElements2 = alloca i32
  store i32 %nElements, i32* %nElements2
  %i = alloca i32
  %tmp = load i32*, i32** %arr1
  %arrayval = getelementptr i32, i32* %tmp, i32 0
  store i32 17, i32* %arrayval
  store i32 1, i32* %i
  br label %cond

loop:                                             ; preds = %cond
  call void @writeString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @tmp.5, i32 0, i32 0))
  %tmp3 = load i32, i32* %i
  %subtmp = sub i32 %tmp3, 1
  call void @writeInteger(i32 %subtmp)
  call void @writeString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @tmp.6, i32 0, i32 0))
  %tmp4 = load i32, i32* %i
  %subtmp5 = sub i32 %tmp4, 1
  %tmp6 = load i32*, i32** %arr1
  %accs = getelementptr i32, i32* %tmp6, i32 %subtmp5
  %tmp7 = load i32, i32* %accs
  call void @writeInteger(i32 %tmp7)
  call void @writeChar(i8 10)
  %tmp8 = load i32, i32* %i
  %tmp9 = load i32*, i32** %arr1
  %arrayval10 = getelementptr i32, i32* %tmp9, i32 %tmp8
  %tmp11 = load i32, i32* %nElements2
  %tmp12 = load i32, i32* %i
  %subtmp13 = sub i32 %tmp11, %tmp12
  %tmp14 = load i32, i32* %i
  %subtmp15 = sub i32 %tmp14, 1
  %tmp16 = load i32*, i32** %arr1
  %accs17 = getelementptr i32, i32* %tmp16, i32 %subtmp15
  %tmp18 = load i32, i32* %accs17
  %sremtmp = srem i32 %subtmp13, %tmp18
  store i32 %sremtmp, i32* %arrayval10
  %tmp19 = load i32, i32* %i
  %tmp20 = load i32*, i32** %arr1
  %accs21 = getelementptr i32, i32* %tmp20, i32 %tmp19
  %tmp22 = load i32, i32* %accs21
  %equaltmp = icmp eq i32 %tmp22, 0
  %ifcond = icmp ne i1 %equaltmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %loop
  %tmp23 = load i32, i32* %i
  %tmp24 = load i32*, i32** %arr1
  %arrayval25 = getelementptr i32, i32* %tmp24, i32 %tmp23
  %tmp26 = load i32, i32* %nElements2
  %tmp27 = load i32, i32* %i
  %subtmp28 = sub i32 %tmp26, %tmp27
  store i32 %subtmp28, i32* %arrayval25
  br label %ifcond29

else:                                             ; preds = %loop
  br label %ifcond29

ifcond29:                                         ; preds = %else, %then
  br label %inc

inc:                                              ; preds = %ifcond29
  %tmp30 = load i32, i32* %i
  %addtmp = add i32 %tmp30, 1
  store i32 %addtmp, i32* %i
  %tmp31 = load i32, i32* %i
  %subtmp32 = sub i32 %tmp31, 1
  br label %cond

cond:                                             ; preds = %inc, %smth
  %tmp33 = load i32, i32* %i
  %tmp34 = load i32, i32* %nElements2
  %lttmp = icmp slt i32 %tmp33, %tmp34
  br i1 %lttmp, label %loop, label %after

after:                                            ; preds = %cond
  ret void
}

define void @main() {
smth:
  %a = alloca i32
  %x = alloca i32*
  %malloccall = tail call i8* @malloc(i32 mul (i32 trunc (i64 mul nuw (i64 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i64), i64 42) to i32), i32 42))
  %mallocttmp = bitcast i8* %malloccall to [42 x i32]*
  %tmp = bitcast [42 x i32]* %mallocttmp to i32*
  store i32* %tmp, i32** %x
  %i = alloca i32
  %t = call i32 @Initialize(i32* %a)
  %tmp1 = load i32, i32* %a
  call void @writeInteger(i32 %tmp1)
  call void @writeChar(i8 10)
  call void @SomeFunc(i32 42)
  %tmp2 = load i32*, i32** %x
  %arrayval = getelementptr i32, i32* %tmp2, i32 0
  store i32 17, i32* %arrayval
  store i32 1, i32* %i
  br label %cond

loop:                                             ; preds = %cond
  call void @writeString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp.7, i32 0, i32 0))
  %tmp3 = load i32, i32* %i
  %subtmp = sub i32 %tmp3, 1
  call void @writeInteger(i32 %subtmp)
  call void @writeString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @tmp.8, i32 0, i32 0))
  %tmp4 = load i32, i32* %i
  %subtmp5 = sub i32 %tmp4, 1
  %tmp6 = load i32*, i32** %x
  %accs = getelementptr i32, i32* %tmp6, i32 %subtmp5
  %tmp7 = load i32, i32* %accs
  call void @writeInteger(i32 %tmp7)
  call void @writeChar(i8 10)
  %tmp8 = load i32, i32* %i
  %tmp9 = load i32*, i32** %x
  %arrayval10 = getelementptr i32, i32* %tmp9, i32 %tmp8
  %tmp11 = load i32, i32* %a
  %tmp12 = load i32, i32* %i
  %subtmp13 = sub i32 %tmp11, %tmp12
  %tmp14 = load i32, i32* %i
  %subtmp15 = sub i32 %tmp14, 1
  %tmp16 = load i32*, i32** %x
  %accs17 = getelementptr i32, i32* %tmp16, i32 %subtmp15
  %tmp18 = load i32, i32* %accs17
  %sremtmp = srem i32 %subtmp13, %tmp18
  store i32 %sremtmp, i32* %arrayval10
  %tmp19 = load i32, i32* %i
  %tmp20 = load i32*, i32** %x
  %accs21 = getelementptr i32, i32* %tmp20, i32 %tmp19
  %tmp22 = load i32, i32* %accs21
  %equaltmp = icmp eq i32 %tmp22, 0
  %ifcond = icmp ne i1 %equaltmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %loop
  %tmp23 = load i32, i32* %i
  %tmp24 = load i32*, i32** %x
  %arrayval25 = getelementptr i32, i32* %tmp24, i32 %tmp23
  %tmp26 = load i32, i32* %a
  %tmp27 = load i32, i32* %i
  %subtmp28 = sub i32 %tmp26, %tmp27
  store i32 %subtmp28, i32* %arrayval25
  br label %ifcond29

else:                                             ; preds = %loop
  br label %ifcond29

ifcond29:                                         ; preds = %else, %then
  br label %inc

inc:                                              ; preds = %ifcond29
  %tmp30 = load i32, i32* %i
  %addtmp = add i32 %tmp30, 1
  store i32 %addtmp, i32* %i
  %tmp31 = load i32, i32* %i
  %subtmp32 = sub i32 %tmp31, 1
  br label %cond

cond:                                             ; preds = %inc, %smth
  %tmp33 = load i32, i32* %i
  %tmp34 = load i32, i32* %a
  %lttmp = icmp slt i32 %tmp33, %tmp34
  br i1 %lttmp, label %loop, label %after

after:                                            ; preds = %cond
  ret void
}

declare noalias i8* @malloc(i32)
