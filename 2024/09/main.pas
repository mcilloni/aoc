(* AoC 2024 Day 09 *)
(* Ironically, this Pascal code does a lot of C-style pointer shenanigans *)

Program aoc09;

(* Enable long strings, otherwise strings will be limited to classic Pascal's 256 chars *)
{$H+}

(* Assertions were useful to debug this mess *)
(* {$assertions on} *)

Type AreaType =   (Chunk, Void);

Type Area =   Record
    Fd:   Integer;
    Kind:   AreaType;
    Width:   Integer;
End;

Type Areas =   Record
    List:   Array Of Area;
    Size:   LongInt;
End;

Type Blockmap =   Array Of Integer;

Type IntPtr =   ^Integer;
(* Pascal has weird type rules *)

Type FreeArea =   Record
    Start:   IntPtr;
    Finish:   IntPtr;
End;

Type FreeAreaPtr =   ^FreeArea;

Type FreeList =   Array Of FreeArea;

Function NextChunk(cur: IntPtr; endp: IntPtr):   IntPtr;
Begin
    While cur < endp Do
        Begin
            If cur^ <> -1 Then
                Exit(cur);

            Inc(cur)
        End;

    NextChunk := endp
End;

Function NextVoid(cur: IntPtr; endp: IntPtr):   IntPtr;
Begin
    While cur < endp Do
        Begin
            If cur^ = -1 Then
                Exit(cur);

            Inc(cur);
        End;

    NextVoid := endp
End;

Function PrecChunk(cur: IntPtr; start: IntPtr):   IntPtr;
Begin
    While cur >= start Do
        Begin
            Dec(cur);

            If cur^ <> -1 Then
                Exit(cur);
        End;

    PrecChunk := start
End;

Function Checksum(Var bmap: Blockmap):   Int64;

Var 
    i:   LongInt;
    cur:   Integer;

Begin
    Checksum := 0;

    For i := 0 To Length(bmap) - 1 Do
        Begin
            cur := bmap[i];

            If cur <> -1 Then
                Checksum := Checksum + cur * i
        End;
End;

Procedure CompactBlockmap(Var bmap: Blockmap);

Var 
    cur:   ^Integer;
    endp:   ^Integer;

Begin
    cur := @bmap[0];
    endp := @bmap[Length(bmap)];

    cur := NextVoid(cur, endp);
    endp := PrecChunk(endp, cur);

    While cur < endp Do
        Begin
            Assert((cur <> Nil) and (endp <> Nil) and (cur^ = -1) and (endp^ <> -1));

            cur^ := endp^;
            endp^ := -1;

            cur := NextVoid(cur, endp);
            endp := PrecChunk(endp, cur)
        End
End;

Procedure DumpAreas(Var areas: Areas);

Var 
    c:   Char;
    i:   Integer;
    cur:   Area;

Begin
    For i := 0 To Length(areas.List) - 1 Do
        Begin
            cur := areas.List[i];
            Write('#', i, ': (', cur.Kind);

            If cur.Kind = Chunk Then
                Write(', Fd: ', cur.Fd);

            WriteLn(', Width: ', cur.Width, ')');
        End;

    For cur In areas.List Do
        Begin
            If cur.Kind = Chunk Then
                c := Chr(Ord('0') + cur.Fd)
            Else
                c := '.';

            For i := 1 To cur.Width Do
                Write(c);

        End;

    WriteLn(LineEnding + 'Total: ', areas.Size);
End;

Procedure DumpBlockmap(Var bmap: Blockmap);

Var 
    i:   LongInt;

Begin
    Write('[');

    For i := 0 To Length(bmap) - 1 Do
        Begin
            If i <> 0 Then
                Write(', ');

            Write(bmap[i]);
        End;

    WriteLn(']')
End;

Procedure DumpBlockmapAsText(Var bmap: Blockmap);

Var 
    n:   Integer;
    i:   LongInt;

Begin
    For i := 0 To Length(bmap) - 1 Do
        Begin
            n := bmap[i];

            If n = -1 Then
                Write('.')
            Else
                Write(n)
        End;

    WriteLn()
End;

Procedure DumpFreeListFor(Var blist: Blockmap; Var flist: FreeList);

Var 
    i:   Integer;
    cur:   FreeArea;
    root:   IntPtr;
    start, finish:   SizeInt;

Begin
    root := @blist[0];

    For i := 0 To Length(flist) - 1 Do
        Begin
            cur := flist[i];
            start := cur.Start - root;
            finish := cur.Finish - root;

            WriteLn('Free area #', i, '(size = ', finish - start, '): [', start, ', ', finish, ')')
        End
End;

Function FreeAreaSize(area: FreeArea):   SizeInt;
Begin
    FreeAreaSize := area.Finish - area.Start
End;

Function FindFreeAreas(Var bmap: Blockmap):   FreeList;

Var 
    cur:   ^Integer;
    endp:   ^Integer;
    nareas:   Integer =   0;
Begin
    cur := @bmap[0];
    endp := @bmap[Length(bmap)];

    FindFreeAreas := Nil;

    While cur < endp Do
        Begin
            cur := NextVoid(cur, endp);

            If cur = endp Then
                Break;

            If FindFreeAreas = Nil Then
                SetLength(FindFreeAreas, 7) (* arbitrary initial size *)
            Else If nareas >= Length(FindFreeAreas) Then
                     SetLength(FindFreeAreas, Length(FindFreeAreas) * 2);

            FindFreeAreas[nareas].Start := cur;

            cur := NextChunk(cur, endp);
            FindFreeAreas[nareas].Finish := cur; (* freeareas are [start, finish) *)

            Inc(nareas)
        End;

    SetLength(FindFreeAreas, nareas)
End;

Function FindNextFreeArea(Var cur: FreeAreaPtr; Var endp: FreeAreaPtr):   FreeAreaPtr;
Begin
    While cur < endp Do
        Begin
            If FreeAreaSize(cur^) > 0 Then
                Exit(cur);

            Inc(cur)
        End;

    FindNextFreeArea := endp
End;

Function MakeBlockmap(Var areas: Areas):   Blockmap;

Var 
    blk_p:   ^Integer;
    cur:   Area;
    i:   Integer;
Begin
    MakeBlockmap := Nil;
    SetLength(MakeBlockmap, areas.Size);

    blk_p := @MakeBlockmap[0];

    For cur In areas.List Do
        Begin
            For i := 1 To cur.Width Do
                Begin
                    blk_p^ := cur.Fd;

                    Inc(blk_p)
                End
        End
End;

Function ValidateFreeAreas(Var freeAreas: FreeList):   Boolean;

Var 
    i:   Integer;
    a:   FreeArea;
    cur:   IntPtr;

Begin
    cur := Nil;

    For i := 0 To Length(freeAreas) - 1 Do
        Begin
            a := freeAreas[i];
            If (cur <> Nil) And (cur >= a.Start) Then
                Exit(False); (* the areas are not sorted *)

            If a.Start >= a.Finish Then
                Exit(False); (* invalid area *)

            cur := a.Start;

            While cur < a.Finish Do
                Begin
                    If cur^ <> -1 Then
                        Exit(False);

                    Inc(cur)
                End
        End;

    ValidateFreeAreas := True
End;

Function MoveToFreeArea(Var dest: FreeAreaPtr; src: IntPtr; size: SizeInt):   SizeInt;

Var 
    freeSize:   SizeInt;
    srcEnd:   IntPtr;
    destPtr:   IntPtr;
Begin
    freeSize := FreeAreaSize(dest^);

    If freeSize < size Then
        Exit(-1);
(* not enough space *)

    destPtr := dest^.Start;
    If destPtr >= src Then
        Exit(-2);
(* the move would push src to the right, which is not allowed *)

    srcEnd := src + size;

    While src < srcEnd Do
        Begin
            Assert(destPtr < dest^.Finish);
            Assert(src^ <> -1);
            Assert(destPtr^ = -1);

            destPtr^ := src^;
            src^ := -1;

            Inc(destPtr);
            Inc(src)
        End;

    Assert(destPtr <= dest^.Finish);

    dest^.Start := destPtr;

    MoveToFreeArea := FreeAreaSize(dest^)
End;

Function MakeCompactedBlockmap(Var areas: Areas) :   Blockmap;

Var 
    toMove:   Area;
    areaIx:   Integer;
    bmap:   Blockmap;

    bmapCur:   IntPtr;

    freeAreas:   FreeList;

    firstFreeArea, freeAreaCur, freeAreaEnd:   FreeAreaPtr;

    freeSize:   SizeInt;
Begin
    bmap := MakeBlockmap(areas);
    bmapCur := @bmap[Length(bmap)];
    freeAreas := FindFreeAreas(bmap);

    Assert(ValidateFreeAreas(freeAreas));

    firstFreeArea := @freeAreas[0];
    freeAreaEnd := @freeAreas[Length(freeAreas)];

    For areaIx := Length(areas.List) - 1 Downto 0 Do
        Begin
            toMove := areas.List[areaIx];

        (* match the chunk in the bmap *)
            bmapCur := bmapCur - toMove.Width;
            Assert(bmapCur >= @bmap[0]);

            If toMove.Kind <> Chunk Then
                Continue;

            If bmapCur < firstFreeArea^.Start Then
                (* we've compacted the blockmap *)
                Exit(bmap);

            freeAreaCur := firstFreeArea;
            While freeAreaCur < freeAreaEnd Do
                Begin
                    (* attempt moving to the areas. If -1, go to the next *)
                    freeSize := MoveToFreeArea(freeAreaCur, bmapCur, toMove.Width);
                    Case freeSize Of 
                        -1:   Inc(freeAreaCur); (* not enough space, try the next one *)
                        -2:   Break (* the move would push the area to the right, which is not allowed *)
                              Else
                                  Begin
                                      Assert(freeSize >= 0);
                    (* free area found, move happened *)

                                        If (freeSize = 0) And (freeAreaCur = firstFreeArea) Then
                                            (* the free area at array start is exhausted, never consider it again *)
                                            Inc(firstFreeArea);

                                      Break
                                  End
                    End
                End;

            (* check if we've exhausted the free blocks *)
            If firstFreeArea = freeAreaEnd Then
                Break;

        End;

    (* It's highly unlikely that we're going to hit this point. Still, put an assert for good measure *)
    Assert(false);

    MakeCompactedBlockmap := bmap
End;

Function ParseInput(Const input: String):   Areas;

Var 
    c:   Char;
    fd:   Integer =   0;
    cur:   ^Area;
    used:   Boolean;

Begin
    used := True;

    ParseInput.Size := 0;
    ParseInput.List := Nil;
    SetLength(ParseInput.List, Length(input));

    cur := @ParseInput.List[0];

    For c In input Do
        Begin
            cur^.Width := Ord(c) - Ord('0');

            If used Then
                Begin
                    cur^.Fd := fd;
                    Inc(fd);

                    cur^.Kind := Chunk
                End
            Else
                Begin
                    cur^.Fd := -1;
                    cur^.Kind := Void;
                End;

            ParseInput.Size := ParseInput.Size + cur^.Width;

            Inc(cur);
            used := Not used
        End
End;

Function ReadInput(filename: String):   string;

Var 
    f:   TextFile;

Begin
    Assign(f, filename);
    Reset(f);

    ReadLn(f, ReadInput);

    Close(f)
End;

Var 
    input:   String;
    aList:   Areas;
    map:   Blockmap;

Begin
    If paramCount() <> 1 Then
        Begin
            WriteLn(StdErr, 'error: wrong number of arguments' + LineEnding + 'usage: aoc09 FILE');
            ExitCode := 1;

            Exit;
        End;

    input := ReadInput(paramStr(1));

    aList := ParseInput(input);

    map := MakeBlockmap(aList);

    CompactBlockmap(map);

    WriteLn('Part 1 = ', Checksum(map));

    map := MakeCompactedBlockmap(aList);

    WriteLn('Part 2 = ', Checksum(map))
End.
