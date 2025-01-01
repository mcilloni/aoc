! AoC 2024 Day 8
! I'm no expert in Fortran, so this may contain some suboptimal code.

module group_mod
    implicit none

    type point
        integer :: x, y
    end type point

    interface operator(+)
        module procedure point_add
    end interface

    interface operator(-)
        module procedure point_neg
        module procedure point_sub
    end interface

    interface operator(==)
        module procedure point_eq
    end interface

    interface operator(.in.)
        module procedure point_in
    end interface

    type points
        type(point), allocatable :: points(:)
        integer :: length = 0
    contains
        procedure, public :: add => points_add
        procedure, public :: dump => points_dump
        procedure, public :: find => points_find
        procedure, public :: get => points_get

        procedure, private:: cap => points_cap
        procedure, private:: grow => points_grow
    end type points

    type, extends(points) :: group
        character :: kind

    contains
        procedure, public :: dump => group_dump
    end type group

    interface group
        module procedure group_cons
    end interface group

contains

    function group_cons(kind) result(inst)
        character, intent(in) :: kind
        type(group) :: inst

        inst%kind = kind
    end function group_cons

    subroutine group_dump(self)
        class(group), intent(in) :: self

        write (*, fmt='("`", a, "`: ")', advance="no") self%kind

        call self%points%dump()
    end subroutine group_dump

    subroutine points_add(self, p, exists)
        class(points), intent(inout) :: self
        type(point), intent(in) :: p
        logical, intent(out), optional :: exists

        integer :: i

        i = self%find(p)

        if (i >= 0) then
            if (present(exists)) then
                exists = .true.
            end if

            return
        end if

        if (self%length >= self%cap()) then
            call self%grow()
        end if

        self%length = self%length+1

        self%points(self%length) = p
    end subroutine points_add

    function points_cap(self) result(cap)
        class(points), intent(in) :: self
        integer :: cap

        if (allocated(self%points)) then
            cap = size(self%points)
        else
            cap = 0
        end if
    end function points_cap

    subroutine points_dump(self)
        class(points), intent(in) :: self

        integer :: i

        do i = 1, self%length
            write (*, fmt='(*(a, i0, a, i0, a))', advance="no") "  (", self%points(i)%x, ", ", self%points(i)%y, ") "
        end do
    end subroutine points_dump

    function points_find(self, p) result(i)
        class(points), intent(in) :: self
        type(point), intent(in) :: p

        integer :: i

        i = 0

        do i = 1, self%length
            if (self%points(i) == p) then
                return
            end if
        end do

        i = -1
    end function points_find

    function points_get(self, n) result(p)
        class(points), intent(in) :: self
        integer, intent(in) :: n
        type(point) :: p

        p = self%points(n)
    end function points_get

    subroutine points_grow(self)
        class(points), intent(inout) :: self
        type(point), allocatable :: tmp(:)

        integer :: old_cap, new_cap

        old_cap = self%cap()

        if (old_cap == 0) then
            new_cap = 6 ! arbitrary
        else
            new_cap = size(self%points)*3/2
        end if

        call move_alloc(self%points, tmp)

        allocate (self%points(new_cap))

        if (old_cap > 0) then
            self%points(1:self%length) = tmp
        end if
    end subroutine points_grow

    pure function point_add(p1, p2) result(res)
        type(point), intent(in) :: p1, p2
        type(point) :: res

        res = point(p1%x+p2%x, p1%y+p2%y)
    end function point_add

    pure function point_eq(p1, p2) result(res)
        type(point), intent(in) :: p1, p2
        logical :: res

        res = p1%x == p2%x .and. p1%y == p2%y
    end function point_eq

    pure function point_in(p, matrix) result(res)
        type(point), intent(in) :: p
        character, intent(in) :: matrix(:, :)
        logical :: res

        res = p%x >= 1 .and. p%y >= 1 .and. p%x <= size(matrix, dim=1) .and. p%y <= size(matrix, dim=2)
    end function point_in

    pure function point_neg(p) result(res)
        type(point), intent(in) :: p
        type(point) :: res

        res = point(-p%x, -p%y)
    end function point_neg

    pure function point_sub(p1, p2) result(res)
        type(point), intent(in) :: p1, p2
        type(point) :: res

        res = point(p1%x-p2%x, p1%y-p2%y)
    end function point_sub

end module group_mod

module glist_mod
    use group_mod
    implicit none

    type match
        type(group), pointer :: grp
        logical :: valid = .false.
    end type match

    type group_list
        type(group), pointer :: groups(:)
        integer :: length = 0, cap = 0

    contains
        procedure, public :: add_to => group_list_add_to
        procedure, public :: dump => group_list_dump
        procedure, public :: find => group_list_find
        procedure, public :: get => group_list_get
        procedure, private:: grow => group_list_grow
        final :: group_list_finalize
    end type group_list

contains

    subroutine group_list_add_to(self, id, p)
        class(group_list), intent(inout) :: self
        character, intent(in) :: id
        type(point), intent(in) :: p

        type(match) :: m
        type(group), pointer :: grp

        m = self%find(id)

        if (m%valid) then
            grp => m%grp
        else
            if (self%cap <= self%length) then
                call self%grow()
            end if

            self%length = self%length+1

            self%groups(self%length) = group(id)

            grp => self%groups(self%length)
        end if

        call grp%add(p)

    end subroutine group_list_add_to

    subroutine group_list_dump(self)
        class(group_list), intent(in) :: self

        integer :: i

        do i = 1, self%length
            call self%groups(i)%dump()
            print *
        end do
    end subroutine group_list_dump

    function group_list_get(self, n) result(g)
        class(group_list), intent(in) :: self
        integer, intent(in) :: n
        type(group), pointer :: g

        ! unchecked
        g => self%groups(n)
    end function group_list_get

    subroutine group_list_grow(self)
        class(group_list), intent(inout) :: self
        type(group), pointer :: tmp(:)

        integer :: new_cap

        if (self%cap == 0) then
            new_cap = 6 ! arbitrary
        else
            new_cap = self%cap*3/2
        end if

        tmp => self%groups

        allocate (self%groups(new_cap))

        if (self%cap > 0) then
            self%groups(1:self%length) = tmp

            deallocate (tmp)
        end if

        self%cap = new_cap
    end subroutine group_list_grow

    subroutine group_list_finalize(self)
        type(group_list), intent(inout) :: self

        if (self%cap > 0) then
            deallocate (self%groups)
        end if
    end subroutine group_list_finalize

    function group_list_find(self, id) result(m)
        class(group_list), intent(in) :: self
        character, intent(in) :: id
        type(match) :: m

        integer :: i

        do i = 1, self%length
            if (self%groups(i)%kind == id) then
                m%grp => self%groups(i)
                m%valid = .true.
                return
            end if
        end do
    end function group_list_find

end module glist_mod

program main
    use glist_mod
    use group_mod
    use iso_fortran_env
    implicit none

    call main_logic()
contains

    subroutine add_if_inside(p, matrix, nodes)
        type(point), intent(in) :: p
        character, intent(in) :: matrix(:, :)
        type(points), intent(inout) :: nodes

        if (p.in.matrix) then
            call nodes%add(p)
        end if
    end subroutine add_if_inside

    subroutine add_while_inside(p, diff, matrix, nodes)
        type(point), intent(in) :: p, diff
        character, intent(in) :: matrix(:, :)
        type(points), intent(inout) :: nodes

        type(point) :: cur

        cur = p

        do
            if (.not. (cur.in.matrix)) then
                exit
            end if

            call nodes%add(cur)

            cur = cur+diff
        end do

    end subroutine add_while_inside

    subroutine dump_matrix(matrix)
        character, intent(in) :: matrix(:, :)
        integer :: i, j

        print '(*(a), a)', ((matrix(i, j), " ", j=1, size(matrix, dim=1)), new_line("A"), i=1, size(matrix, dim=2))
    end subroutine dump_matrix

    subroutine dump_nodes_in(matrix, nodes)
        character, intent(in) :: matrix(:, :)
        type(points), intent(in) :: nodes

        character, allocatable :: clone(:, :)
        integer :: i
        type(point) :: p

        allocate (clone, source=matrix)

        write (*, fmt='("Found ", i0, " nodes: ", a)', advance="no") nodes%length, new_line('a')
        do i = 1, nodes%length
            p = nodes%get(i)

            write (*, fmt='(a, i0, a, i0, a)', advance="no") "(", p%x, ", ", p%y, ") "

            if (clone(p%x, p%y) == '.') then
                clone(p%x, p%y) = '#'
            end if
        end do

        print *

        call dump_matrix(clone)
    end subroutine dump_nodes_in

    subroutine errln(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a, a)') "error: ", trim(message)
    end subroutine errln

    subroutine find_groups(matrix, grps)
        character, intent(in) :: matrix(:, :)
        type(group_list), intent(out) :: grps

        character :: c
        integer :: i, j

        do i = lbound(matrix, dim=1), ubound(matrix, dim=1)
            do j = lbound(matrix, dim=2), ubound(matrix, dim=2)
                c = matrix(i, j)

                if (c /= '.') then
                    call grps%add_to(c, point(i, j))
                end if
            end do
        end do
    end subroutine find_groups

    subroutine load_input(file, grid, iostat)
        character(len=*), intent(in) :: file
        character, dimension(:, :), allocatable, intent(out) :: grid
        integer, intent(out) :: iostat

        integer :: rows, cols, io, i

        call measure_size(file, rows, cols, iostat)
        if (iostat /= 0) then
            return
        end if

        print '(A, i0, A, i0)', "rows=", rows, ", cols=", cols

        open (newunit=io, file=file, status="old", action="read", iostat=iostat)
        if (iostat /= 0) then
            return
        end if

        allocate (grid(rows, cols))

        read_loop: do i = 1, rows
            read (io, '(*(a))', iostat=iostat) grid(i, :)
            select case (iostat)
            case (0)
                cycle read_loop
            case (iostat_end)
                iostat = 0
                exit read_loop
            case default
                return
            end select
        end do read_loop

        close (io)
    end subroutine load_input

    subroutine main_logic()

        character, dimension(:, :), allocatable :: data
        character(len=100) :: fname
        integer :: status
        type(group_list) :: groups
        type(points) :: nodes

        if (command_argument_count() /= 1) then
            call errln('Usage: main <name>')
            stop 1
        end if

        call get_command_argument(1, fname)

        call load_input(fname, data, status)

        if (status /= 0) then
            call errln('Error reading file')
            stop 1
        end if

        call dump_matrix(data)

        call find_groups(data, groups)

        call groups%dump()

        print *

        call project_nodes_p1(groups, data, nodes)

        print '(a, a)', "# part 1", new_line('a')
        call dump_nodes_in(data, nodes)

        nodes = points()

        call project_nodes_p2(groups, data, nodes)

        print '(a, a)', "# part 2", new_line('a')
        call dump_nodes_in(data, nodes)
    end subroutine main_logic

    subroutine measure_size(fname, rows, cols, iostat)
        character(len=*), intent(in) :: fname
        integer, intent(out) :: rows, cols, iostat
        character :: c

        integer :: io

        rows = 0
        cols = 0

        open (newunit=io, access='stream', file=fname, form='unformatted', iostat=iostat, status="old", action="read")
        if (iostat /= 0) then
            return
        end if

        count_loop: do
            read (io, iostat=iostat) c
            if (is_iostat_end(iostat)) then
                iostat = 0
                exit count_loop
            else if (iostat /= 0) then
                return
            end if

            if (c == new_line('a')) then
                rows = rows+1
            elseif (rows == 1) then
                cols = cols+1
            end if
        end do count_loop

        close (io)
    end subroutine measure_size

    subroutine project_group_nodes_p1(grp, matrix, nodes)
        type(group), intent(in) :: grp
        character, intent(in) :: matrix(:, :)
        type(points), intent(inout) :: nodes

        integer :: i, j
        type(point) :: diff

        do i = 1, grp%length
            do j = i+1, grp%length
                diff = grp%get(i)-grp%get(j)

                call add_if_inside(grp%get(i)+diff, matrix, nodes)
                call add_if_inside(grp%get(j)-diff, matrix, nodes)
            end do
        end do
    end subroutine project_group_nodes_p1

    subroutine project_group_nodes_p2(grp, matrix, nodes)
        type(group), intent(in) :: grp
        character, intent(in) :: matrix(:, :)
        type(points), intent(inout) :: nodes

        integer :: i, j
        type(point) :: diff

        do i = 1, grp%length
            do j = i+1, grp%length
                diff = grp%get(i)-grp%get(j)

                call add_while_inside(grp%get(i), diff, matrix, nodes)
                call add_while_inside(grp%get(j), -diff, matrix, nodes)
            end do
        end do

    end subroutine project_group_nodes_p2

    subroutine project_nodes_p1(groups, matrix, nodes)
        type(group_list), intent(in) :: groups
        character, intent(in) :: matrix(:, :)
        type(points), intent(out) :: nodes

        integer :: i

        do i = 1, groups%length
            call project_group_nodes_p1(groups%get(i), matrix, nodes)
        end do

    end subroutine project_nodes_p1

    subroutine project_nodes_p2(groups, matrix, nodes)
        type(group_list), intent(in) :: groups
        character, intent(in) :: matrix(:, :)
        type(points), intent(out) :: nodes

        integer :: i

        do i = 1, groups%length
            call project_group_nodes_p2(groups%get(i), matrix, nodes)
        end do

    end subroutine project_nodes_p2

end program main
