; ModuleID = 'My cmp '
source_filename = "My cmp "

@tmp = private unnamed_addr constant [28 x i8] c"Give size of array to sort\0A\00"
@tmp.1 = private unnamed_addr constant [25 x i8] c"Give elements of array.\0A\00"

declare i32 @trunc(x86_fp80)

declare i32 @round(x86_fp80)

declare i32 @ord(i8)

declare i8 @chr(i32)

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

define void @merge(i32* %a, i32 %n, i32 %m) {
smth:
  %a1 = alloca i32*
  store i32* %a, i32** %a1
  %n2 = alloca i32
  store i32 %n, i32* %n2
  %m3 = alloca i32
  store i32 %m, i32* %m3
  %i = alloca i32
  %j = alloca i32
  %k = alloca i32
  %x = alloca i32*
  %tmp = load i32, i32* %n2
  %mallocsize = mul i32 %tmp, ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32)
  %malloccall = tail call i8* @malloc(i32 %mallocsize)
  %tmp4 = bitcast i8* %malloccall to i32*
  store i32* %tmp4, i32** %x
  store i32 0, i32* %i
  %tmp5 = load i32, i32* %m3
  store i32 %tmp5, i32* %j
  store i32 0, i32* %k
  br label %cond

loop:                                             ; preds = %cond
  %tmp6 = load i32, i32* %k
  %tmp7 = load i32*, i32** %x
  %arrayval = getelementptr i32, i32* %tmp7, i32 %tmp6
  %tmp8 = load i32, i32* %j
  %tmp9 = load i32, i32* %n2
  %equaltmp = icmp eq i32 %tmp8, %tmp9
  %ifcond = icmp ne i1 %equaltmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %loop
  %tmp10 = load i32, i32* %i
  %addtmp = add i32 %tmp10, 1
  store i32 %addtmp, i32* %i
  %tmp11 = load i32, i32* %i
  %subtmp = sub i32 %tmp11, 1
  %tmp12 = load i32*, i32** %a1
  %accs = getelementptr i32, i32* %tmp12, i32 %subtmp
  %tmp13 = load i32, i32* %accs
  br label %ifcond55

else:                                             ; preds = %loop
  %tmp14 = load i32, i32* %i
  %tmp15 = load i32, i32* %m3
  %equaltmp16 = icmp eq i32 %tmp14, %tmp15
  %ifcond17 = icmp ne i1 %equaltmp16, false
  br i1 %ifcond17, label %then18, label %else26

then18:                                           ; preds = %else
  %tmp19 = load i32, i32* %j
  %addtmp20 = add i32 %tmp19, 1
  store i32 %addtmp20, i32* %j
  %tmp21 = load i32, i32* %j
  %subtmp22 = sub i32 %tmp21, 1
  %tmp23 = load i32*, i32** %a1
  %accs24 = getelementptr i32, i32* %tmp23, i32 %subtmp22
  %tmp25 = load i32, i32* %accs24
  br label %ifcond53

else26:                                           ; preds = %else
  %tmp27 = load i32, i32* %j
  %tmp28 = load i32*, i32** %a1
  %accs29 = getelementptr i32, i32* %tmp28, i32 %tmp27
  %tmp30 = load i32, i32* %i
  %tmp31 = load i32*, i32** %a1
  %accs32 = getelementptr i32, i32* %tmp31, i32 %tmp30
  %tmp33 = load i32, i32* %accs29
  %tmp34 = load i32, i32* %accs32
  %lttmp = icmp slt i32 %tmp33, %tmp34
  %ifcond35 = icmp ne i1 %lttmp, false
  br i1 %ifcond35, label %then36, label %else44

then36:                                           ; preds = %else26
  %tmp37 = load i32, i32* %j
  %addtmp38 = add i32 %tmp37, 1
  store i32 %addtmp38, i32* %j
  %tmp39 = load i32, i32* %j
  %subtmp40 = sub i32 %tmp39, 1
  %tmp41 = load i32*, i32** %a1
  %accs42 = getelementptr i32, i32* %tmp41, i32 %subtmp40
  %tmp43 = load i32, i32* %accs42
  br label %ifcond52

else44:                                           ; preds = %else26
  %tmp45 = load i32, i32* %i
  %addtmp46 = add i32 %tmp45, 1
  store i32 %addtmp46, i32* %i
  %tmp47 = load i32, i32* %i
  %subtmp48 = sub i32 %tmp47, 1
  %tmp49 = load i32*, i32** %a1
  %accs50 = getelementptr i32, i32* %tmp49, i32 %subtmp48
  %tmp51 = load i32, i32* %accs50
  br label %ifcond52

ifcond52:                                         ; preds = %else44, %then36
  %iftmp = phi i32 [ %tmp43, %then36 ], [ %tmp51, %else44 ]
  br label %ifcond53

ifcond53:                                         ; preds = %ifcond52, %then18
  %iftmp54 = phi i32 [ %tmp25, %then18 ], [ %iftmp, %ifcond52 ]
  br label %ifcond55

ifcond55:                                         ; preds = %ifcond53, %then
  %iftmp56 = phi i32 [ %tmp13, %then ], [ %iftmp54, %ifcond53 ]
  store i32 %iftmp56, i32* %arrayval
  br label %inc

inc:                                              ; preds = %ifcond55
  %tmp57 = load i32, i32* %k
  %addtmp58 = add i32 %tmp57, 1
  store i32 %addtmp58, i32* %k
  %tmp59 = load i32, i32* %k
  %subtmp60 = sub i32 %tmp59, 1
  br label %cond

cond:                                             ; preds = %inc, %smth
  %tmp61 = load i32, i32* %k
  %tmp62 = load i32, i32* %n2
  %lttmp63 = icmp slt i32 %tmp61, %tmp62
  br i1 %lttmp63, label %loop, label %after

after:                                            ; preds = %cond
  store i32 0, i32* %i
  br label %cond66

loop64:                                           ; preds = %cond66
  %tmp68 = load i32, i32* %i
  %tmp69 = load i32*, i32** %a1
  %arrayval70 = getelementptr i32, i32* %tmp69, i32 %tmp68
  %tmp71 = load i32, i32* %i
  %tmp72 = load i32*, i32** %x
  %accs73 = getelementptr i32, i32* %tmp72, i32 %tmp71
  %tmp74 = load i32, i32* %accs73
  store i32 %tmp74, i32* %arrayval70
  br label %inc65

inc65:                                            ; preds = %loop64
  %tmp75 = load i32, i32* %i
  %addtmp76 = add i32 %tmp75, 1
  store i32 %addtmp76, i32* %i
  %tmp77 = load i32, i32* %i
  %subtmp78 = sub i32 %tmp77, 1
  br label %cond66

cond66:                                           ; preds = %inc65, %after
  %tmp79 = load i32, i32* %i
  %tmp80 = load i32, i32* %n2
  %lttmp81 = icmp slt i32 %tmp79, %tmp80
  br i1 %lttmp81, label %loop64, label %after67

after67:                                          ; preds = %cond66
  %tmp82 = load i32*, i32** %x
  %0 = bitcast i32* %tmp82 to i8*
  tail call void @free(i8* %0)
  ret void
}

declare noalias i8* @malloc(i32)

declare void @free(i8*)

define void @merge_sort(i32* %a, i32 %n) {
smth:
  %a1 = alloca i32*
  store i32* %a, i32** %a1
  %n2 = alloca i32
  store i32 %n, i32* %n2
  %m = alloca i32
  %tmp = load i32, i32* %n2
  %lttmp = icmp slt i32 %tmp, 2
  %ifcond = icmp ne i1 %lttmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %smth
  ret void
  br label %ifcond3

else:                                             ; preds = %smth
  br label %ifcond3

ifcond3:                                          ; preds = %else, %then
  %tmp4 = load i32, i32* %n2
  %divtmp = sdiv i32 %tmp4, 2
  store i32 %divtmp, i32* %m
  %tmp5 = load i32*, i32** %a1
  %tmp6 = load i32, i32* %m
  call void @merge_sort(i32* %tmp5, i32 %tmp6)
  %tmp7 = load i32*, i32** %a1
  %tmp8 = load i32, i32* %m
  %tmp9 = load i32, i32* %m
  %tmp10 = load i32*, i32** %a1
  %accs = getelementptr i32, i32* %tmp10, i32 %tmp9
  %tmp11 = load i32, i32* %n2
  %tmp12 = load i32, i32* %m
  %subtmp = sub i32 %tmp11, %tmp12
  call void @merge_sort(i32* %accs, i32 %subtmp)
  %tmp13 = load i32*, i32** %a1
  %tmp14 = load i32, i32* %n2
  %tmp15 = load i32, i32* %m
  call void @merge(i32* %tmp13, i32 %tmp14, i32 %tmp15)
  ret void
}

define void @main() {
smth:
  %a = alloca i32*
  %malloccall = tail call i8* @malloc(i32 mul (i32 trunc (i64 mul nuw (i64 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i64), i64 100) to i32), i32 100))
  %mallocttmp = bitcast i8* %malloccall to [100 x i32]*
  %tmp = bitcast [100 x i32]* %mallocttmp to i32*
  store i32* %tmp, i32** %a
  %point_to_a = alloca i32*
  %n = alloca i32
  %i = alloca i32
  %tmp1 = load i32*, i32** %a
  store i32* %tmp1, i32** %point_to_a
  call void @writeString(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @tmp, i32 0, i32 0))
  %t = call i32 @readInteger()
  store i32 %t, i32* %n
  call void @writeString(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @tmp.1, i32 0, i32 0))
  store i32 0, i32* %i
  br label %cond

loop:                                             ; preds = %cond
  %tmp2 = load i32, i32* %i
  %tmp3 = load i32*, i32** %a
  %arrayval = getelementptr i32, i32* %tmp3, i32 %tmp2
  %t4 = call i32 @readInteger()
  store i32 %t4, i32* %arrayval
  br label %inc

inc:                                              ; preds = %loop
  %tmp5 = load i32, i32* %i
  %addtmp = add i32 %tmp5, 1
  store i32 %addtmp, i32* %i
  %tmp6 = load i32, i32* %i
  %subtmp = sub i32 %tmp6, 1
  br label %cond

cond:                                             ; preds = %inc, %smth
  %tmp7 = load i32, i32* %i
  %tmp8 = load i32, i32* %n
  %lttmp = icmp slt i32 %tmp7, %tmp8
  br i1 %lttmp, label %loop, label %after

after:                                            ; preds = %cond
  store i32 0, i32* %i
  br label %cond11

loop9:                                            ; preds = %cond11
  %tmp13 = load i32, i32* %i
  %tmp14 = load i32*, i32** %a
  %accs = getelementptr i32, i32* %tmp14, i32 %tmp13
  %tmp15 = load i32, i32* %accs
  call void @writeInteger(i32 %tmp15)
  %tmp16 = load i32, i32* %n
  %subtmp17 = sub i32 %tmp16, 1
  %tmp18 = load i32, i32* %i
  %equaltmp = icmp eq i32 %tmp18, %subtmp17
  %ifcond = icmp ne i1 %equaltmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %loop9
  call void @writeChar(i8 10)
  br label %ifcond19

else:                                             ; preds = %loop9
  call void @writeChar(i8 32)
  br label %ifcond19

ifcond19:                                         ; preds = %else, %then
  br label %inc10

inc10:                                            ; preds = %ifcond19
  %tmp20 = load i32, i32* %i
  %addtmp21 = add i32 %tmp20, 1
  store i32 %addtmp21, i32* %i
  %tmp22 = load i32, i32* %i
  %subtmp23 = sub i32 %tmp22, 1
  br label %cond11

cond11:                                           ; preds = %inc10, %after
  %tmp24 = load i32, i32* %i
  %tmp25 = load i32, i32* %n
  %lttmp26 = icmp slt i32 %tmp24, %tmp25
  br i1 %lttmp26, label %loop9, label %after12

after12:                                          ; preds = %cond11
  %tmp27 = load i32*, i32** %a
  %tmp28 = load i32, i32* %n
  call void @merge_sort(i32* %tmp27, i32 %tmp28)
  store i32 0, i32* %i
  br label %cond31

loop29:                                           ; preds = %cond31
  %tmp33 = load i32*, i32** %point_to_a
  %tmp34 = load i32, i32* %tmp33
  call void @writeInteger(i32 %tmp34)
  %tmp35 = load i32, i32* %n
  %subtmp36 = sub i32 %tmp35, 1
  %tmp37 = load i32, i32* %i
  %equaltmp38 = icmp eq i32 %tmp37, %subtmp36
  %ifcond39 = icmp ne i1 %equaltmp38, false
  br i1 %ifcond39, label %then40, label %else41

then40:                                           ; preds = %loop29
  call void @writeChar(i8 10)
  br label %ifcond42

else41:                                           ; preds = %loop29
  call void @writeChar(i8 32)
  br label %ifcond42

ifcond42:                                         ; preds = %else41, %then40
  br label %inc30

inc30:                                            ; preds = %ifcond42
  %tmp43 = load i32, i32* %i
  %addtmp44 = add i32 %tmp43, 1
  store i32 %addtmp44, i32* %i
  %tmp45 = load i32, i32* %i
  %subtmp46 = sub i32 %tmp45, 1
  %tmp47 = load i32*, i32** %point_to_a
  %tmp48 = load i32*, i32** %point_to_a
  %accs49 = getelementptr i32, i32* %tmp48, i32 1
  store i32* %accs49, i32** %point_to_a
  %tmp50 = load i32*, i32** %point_to_a
  %tmp51 = load i32*, i32** %point_to_a
  %accs52 = getelementptr i32, i32* %tmp51, i32 -1
  br label %cond31

cond31:                                           ; preds = %inc30, %after12
  %tmp53 = load i32, i32* %i
  %tmp54 = load i32, i32* %n
  %lttmp55 = icmp slt i32 %tmp53, %tmp54
  br i1 %lttmp55, label %loop29, label %after32

after32:                                          ; preds = %cond31
  ret void
}
