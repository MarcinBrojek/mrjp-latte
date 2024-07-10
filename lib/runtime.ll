; ModuleID = 'runtime.c'
source_filename = "runtime.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx14.0.0"

%struct.string = type { ptr, i32 }

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.2 = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
@.str.3 = private unnamed_addr constant [6 x i8] c"%d[^]\00", align 1
@__stdinp = external global ptr, align 8

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @function_printInt(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %3)
  ret void
}

declare i32 @printf(ptr noundef, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @function_printString(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr inbounds %struct.string, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, ptr noundef %5)
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @function_error() #0 {
  %1 = call i32 (ptr, ...) @printf(ptr noundef @.str.2)
  call void @exit(i32 noundef 1) #8
  unreachable
}

; Function Attrs: noreturn
declare void @exit(i32 noundef) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @function_readInt() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  %3 = call i32 (ptr, ...) @scanf(ptr noundef @.str.3, ptr noundef %1)
  br label %4

4:                                                ; preds = %7, %0
  %5 = load ptr, ptr @__stdinp, align 8
  %6 = call i32 @getc(ptr noundef %5)
  store i32 %6, ptr %2, align 4
  br label %7

7:                                                ; preds = %4
  %8 = load i32, ptr %2, align 4
  %9 = icmp eq i32 %8, 32
  br i1 %9, label %4, label %10, !llvm.loop !5

10:                                               ; preds = %7
  %11 = load i32, ptr %2, align 4
  %12 = icmp ne i32 %11, -1
  br i1 %12, label %13, label %20

13:                                               ; preds = %10
  %14 = load i32, ptr %2, align 4
  %15 = icmp ne i32 %14, 10
  br i1 %15, label %16, label %20

16:                                               ; preds = %13
  %17 = load i32, ptr %2, align 4
  %18 = load ptr, ptr @__stdinp, align 8
  %19 = call i32 @ungetc(i32 noundef %17, ptr noundef %18)
  br label %20

20:                                               ; preds = %16, %13, %10
  %21 = load i32, ptr %1, align 4
  ret i32 %21
}

declare i32 @scanf(ptr noundef, ...) #1

declare i32 @getc(ptr noundef) #1

declare i32 @ungetc(i32 noundef, ptr noundef) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define ptr @__readString() #0 {
  %1 = alloca ptr, align 8
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  %4 = call ptr @malloc(i64 noundef 100) #9
  store ptr %4, ptr %1, align 8
  %5 = load ptr, ptr %1, align 8
  %6 = load ptr, ptr @__stdinp, align 8
  %7 = call ptr @fgets(ptr noundef %5, i32 noundef 100, ptr noundef %6)
  store ptr %7, ptr %2, align 8
  %8 = load ptr, ptr %1, align 8
  %9 = call i64 @strlen(ptr noundef %8)
  %10 = trunc i64 %9 to i32
  store i32 %10, ptr %3, align 4
  %11 = load ptr, ptr %2, align 8
  %12 = icmp eq ptr %11, null
  br i1 %12, label %22, label %13

13:                                               ; preds = %0
  %14 = load ptr, ptr %1, align 8
  %15 = load i32, ptr %3, align 4
  %16 = sub nsw i32 %15, 1
  %17 = sext i32 %16 to i64
  %18 = getelementptr inbounds i8, ptr %14, i64 %17
  %19 = load i8, ptr %18, align 1
  %20 = sext i8 %19 to i32
  %21 = icmp eq i32 %20, 10
  br i1 %21, label %22, label %47

22:                                               ; preds = %13, %0
  %23 = load i32, ptr %3, align 4
  %24 = icmp sgt i32 %23, 0
  br i1 %24, label %25, label %41

25:                                               ; preds = %22
  %26 = load ptr, ptr %1, align 8
  %27 = load i32, ptr %3, align 4
  %28 = sub nsw i32 %27, 1
  %29 = sext i32 %28 to i64
  %30 = getelementptr inbounds i8, ptr %26, i64 %29
  %31 = load i8, ptr %30, align 1
  %32 = sext i8 %31 to i32
  %33 = icmp eq i32 %32, 10
  br i1 %33, label %34, label %41

34:                                               ; preds = %25
  %35 = load i32, ptr %3, align 4
  %36 = sub nsw i32 %35, 1
  store i32 %36, ptr %3, align 4
  %37 = load ptr, ptr %1, align 8
  %38 = load i32, ptr %3, align 4
  %39 = sext i32 %38 to i64
  %40 = getelementptr inbounds i8, ptr %37, i64 %39
  store i8 0, ptr %40, align 1
  br label %41

41:                                               ; preds = %34, %25, %22
  %42 = load ptr, ptr %1, align 8
  %43 = load i32, ptr %3, align 4
  %44 = add nsw i32 %43, 1
  %45 = sext i32 %44 to i64
  %46 = call ptr @realloc(ptr noundef %42, i64 noundef %45) #10
  store ptr %46, ptr %1, align 8
  br label %62

47:                                               ; preds = %13
  %48 = call ptr @__readString()
  store ptr %48, ptr %2, align 8
  %49 = load ptr, ptr %2, align 8
  %50 = call i64 @strlen(ptr noundef %49)
  %51 = trunc i64 %50 to i32
  store i32 %51, ptr %3, align 4
  %52 = load ptr, ptr %1, align 8
  %53 = load i32, ptr %3, align 4
  %54 = add nsw i32 100, %53
  %55 = sext i32 %54 to i64
  %56 = call ptr @realloc(ptr noundef %52, i64 noundef %55) #10
  store ptr %56, ptr %1, align 8
  %57 = load ptr, ptr %1, align 8
  %58 = load ptr, ptr %2, align 8
  %59 = load ptr, ptr %1, align 8
  %60 = call i64 @llvm.objectsize.i64.p0(ptr %59, i1 false, i1 true, i1 false)
  %61 = call ptr @__strcat_chk(ptr noundef %57, ptr noundef %58, i64 noundef %60) #11
  store ptr %61, ptr %1, align 8
  br label %62

62:                                               ; preds = %47, %41
  %63 = load ptr, ptr %1, align 8
  ret ptr %63
}

; Function Attrs: allocsize(0)
declare ptr @malloc(i64 noundef) #3

declare ptr @fgets(ptr noundef, i32 noundef, ptr noundef) #1

declare i64 @strlen(ptr noundef) #1

; Function Attrs: allocsize(1)
declare ptr @realloc(ptr noundef, i64 noundef) #4

; Function Attrs: nounwind
declare ptr @__strcat_chk(ptr noundef, ptr noundef, i64 noundef) #5

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.objectsize.i64.p0(ptr, i1 immarg, i1 immarg, i1 immarg) #6

; Function Attrs: noinline nounwind optnone ssp uwtable
define ptr @function_readString() #0 {
  %1 = alloca ptr, align 8
  %2 = alloca ptr, align 8
  %3 = call ptr @__readString()
  store ptr %3, ptr %1, align 8
  %4 = call ptr @malloc(i64 noundef 16) #9
  store ptr %4, ptr %2, align 8
  %5 = load ptr, ptr %1, align 8
  %6 = load ptr, ptr %2, align 8
  %7 = getelementptr inbounds %struct.string, ptr %6, i32 0, i32 0
  store ptr %5, ptr %7, align 8
  %8 = load ptr, ptr %1, align 8
  %9 = call i64 @strlen(ptr noundef %8)
  %10 = trunc i64 %9 to i32
  %11 = load ptr, ptr %2, align 8
  %12 = getelementptr inbounds %struct.string, ptr %11, i32 0, i32 1
  store i32 %10, ptr %12, align 8
  %13 = load ptr, ptr %2, align 8
  ret ptr %13
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define ptr @function_concat(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca ptr, align 8
  %10 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  store ptr %1, ptr %4, align 8
  %11 = load ptr, ptr %3, align 8
  %12 = getelementptr inbounds %struct.string, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  store ptr %13, ptr %5, align 8
  %14 = load ptr, ptr %4, align 8
  %15 = getelementptr inbounds %struct.string, ptr %14, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  store ptr %16, ptr %6, align 8
  %17 = load ptr, ptr %5, align 8
  %18 = call i64 @strlen(ptr noundef %17)
  %19 = trunc i64 %18 to i32
  store i32 %19, ptr %7, align 4
  %20 = load ptr, ptr %6, align 8
  %21 = call i64 @strlen(ptr noundef %20)
  %22 = trunc i64 %21 to i32
  store i32 %22, ptr %8, align 4
  %23 = load i32, ptr %7, align 4
  %24 = load i32, ptr %8, align 4
  %25 = add nsw i32 %23, %24
  %26 = add nsw i32 %25, 1
  %27 = sext i32 %26 to i64
  %28 = call ptr @calloc(i64 noundef %27, i64 noundef 1) #12
  store ptr %28, ptr %9, align 8
  %29 = load ptr, ptr %9, align 8
  %30 = load ptr, ptr %5, align 8
  %31 = load ptr, ptr %9, align 8
  %32 = call i64 @llvm.objectsize.i64.p0(ptr %31, i1 false, i1 true, i1 false)
  %33 = call ptr @__strcat_chk(ptr noundef %29, ptr noundef %30, i64 noundef %32) #11
  store ptr %33, ptr %9, align 8
  %34 = load ptr, ptr %9, align 8
  %35 = load ptr, ptr %6, align 8
  %36 = load ptr, ptr %9, align 8
  %37 = call i64 @llvm.objectsize.i64.p0(ptr %36, i1 false, i1 true, i1 false)
  %38 = call ptr @__strcat_chk(ptr noundef %34, ptr noundef %35, i64 noundef %37) #11
  store ptr %38, ptr %9, align 8
  %39 = call ptr @malloc(i64 noundef 16) #9
  store ptr %39, ptr %10, align 8
  %40 = load ptr, ptr %9, align 8
  %41 = load ptr, ptr %10, align 8
  %42 = getelementptr inbounds %struct.string, ptr %41, i32 0, i32 0
  store ptr %40, ptr %42, align 8
  %43 = load ptr, ptr %3, align 8
  %44 = getelementptr inbounds %struct.string, ptr %43, i32 0, i32 1
  %45 = load i32, ptr %44, align 8
  %46 = load ptr, ptr %4, align 8
  %47 = getelementptr inbounds %struct.string, ptr %46, i32 0, i32 1
  %48 = load i32, ptr %47, align 8
  %49 = add nsw i32 %45, %48
  %50 = load ptr, ptr %10, align 8
  %51 = getelementptr inbounds %struct.string, ptr %50, i32 0, i32 1
  store i32 %49, ptr %51, align 8
  %52 = load ptr, ptr %10, align 8
  ret ptr %52
}

; Function Attrs: allocsize(0,1)
declare ptr @calloc(i64 noundef, i64 noundef) #7

attributes #0 = { noinline nounwind optnone ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #2 = { noreturn "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #3 = { allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #4 = { allocsize(1) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #6 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #7 = { allocsize(0,1) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #8 = { noreturn }
attributes #9 = { allocsize(0) }
attributes #10 = { allocsize(1) }
attributes #11 = { nounwind }
attributes #12 = { allocsize(0,1) }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 2}
!3 = !{i32 7, !"frame-pointer", i32 2}
!4 = !{!"Homebrew clang version 17.0.6"}
!5 = distinct !{!5, !6}
!6 = !{!"llvm.loop.mustprogress"}
