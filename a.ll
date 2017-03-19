; ModuleID = 'My cmp '

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %a = bitcast i8* %malloccall to i32*
  store i32 1, i32* %a
  ret i32 0
}

declare noalias i8* @malloc(i32)
