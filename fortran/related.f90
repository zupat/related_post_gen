module related_mod
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use json_module, only: json_core, json_value, json_ik, json_lk
    implicit none

    type :: tag_type
        character(len=:), allocatable :: name
    end type tag_type

    type :: post_type
        character(len=:), allocatable :: id
        character(len=:), allocatable :: title
        type(tag_type), allocatable :: tags(:)
    end type post_type

contains

    subroutine read_posts(path, posts)
        character(len=*), intent(in) :: path
        type(post_type), allocatable, intent(out) :: posts(:)

        type(json_core) :: j
        type(json_value), pointer :: root, post
        character(len=:), allocatable :: s
        character(len=:), allocatable :: tag_names(:)
        integer(json_ik), allocatable :: tag_lens(:)
        integer(json_ik) :: n_ik
        integer :: n, i, k, nt
        logical(json_lk) :: found

        call j%initialize(trailing_spaces_significant=.true.)
        call j%load(path, root)
        if (j%failed()) then
            call j%print_error_message(error_unit)
            error stop 1
        end if

        call j%info(root, n_children=n_ik)
        n = int(n_ik)
        allocate(posts(n))

        do i = 1, n
            call j%get_child(root, int(i, json_ik), post)

            call j%get(post, '_id', s, found)
            if (.not. found) error stop 'missing _id'
            posts(i)%id = s

            call j%get(post, 'title', s, found)
            if (.not. found) error stop 'missing title'
            posts(i)%title = s

            call j%get(post, 'tags', tag_names, tag_lens, found)
            if (.not. found) error stop 'missing tags'
            nt = size(tag_names)
            allocate(posts(i)%tags(nt))
            do k = 1, nt
                posts(i)%tags(k)%name = tag_names(k)(1:int(tag_lens(k)))
            end do
        end do

        call j%destroy(root)
    end subroutine read_posts

    integer function find_tag(unique, n_unique, name) result(id)
        type(tag_type), intent(in) :: unique(:)
        integer, intent(in) :: n_unique
        character(len=*), intent(in) :: name
        integer :: k

        id = 0
        do k = 1, n_unique
            if (unique(k)%name == name) then
                id = k
                return
            end if
        end do
    end function find_tag

    subroutine add_unique(unique, n_unique, name)
        type(tag_type), allocatable, intent(inout) :: unique(:)
        integer, intent(inout) :: n_unique
        character(len=*), intent(in) :: name

        type(tag_type), allocatable :: tmp(:)
        integer :: cap, k

        if (.not. allocated(unique)) allocate(unique(8))
        if (n_unique == size(unique)) then
            cap = size(unique) * 2
            allocate(tmp(cap))
            do k = 1, n_unique
                tmp(k)%name = unique(k)%name
            end do
            call move_alloc(tmp, unique)
        end if

        n_unique = n_unique + 1
        unique(n_unique)%name = name
    end subroutine add_unique

    subroutine build_index(posts, unique, n_unique, post_tag_start, post_tag_ids, &
                           tag_start, tag_posts)
        type(post_type), intent(in) :: posts(:)
        type(tag_type), allocatable, intent(out) :: unique(:)
        integer, intent(out) :: n_unique
        integer, allocatable, intent(out) :: post_tag_start(:)
        integer, allocatable, intent(out) :: post_tag_ids(:)
        integer, allocatable, intent(out) :: tag_start(:)
        integer, allocatable, intent(out) :: tag_posts(:)

        integer, allocatable :: cursor(:)
        integer :: n, i, k, pos, total, id

        n = size(posts)
        total = 0
        do i = 1, n
            total = total + size(posts(i)%tags)
        end do

        allocate(post_tag_start(n + 1))
        allocate(post_tag_ids(total))

        n_unique = 0
        pos = 0
        post_tag_start(1) = 1
        do i = 1, n
            do k = 1, size(posts(i)%tags)
                pos = pos + 1
                id = find_tag(unique, n_unique, posts(i)%tags(k)%name)
                if (id == 0) then
                    call add_unique(unique, n_unique, posts(i)%tags(k)%name)
                    id = n_unique
                end if
                post_tag_ids(pos) = id
            end do
            post_tag_start(i + 1) = pos + 1
        end do

        allocate(tag_start(n_unique + 1))
        tag_start = 0
        do pos = 1, total
            tag_start(post_tag_ids(pos) + 1) = tag_start(post_tag_ids(pos) + 1) + 1
        end do
        tag_start(1) = 1
        do id = 1, n_unique
            tag_start(id + 1) = tag_start(id) + tag_start(id + 1)
        end do

        allocate(tag_posts(total))
        allocate(cursor(n_unique))
        cursor = tag_start(1:n_unique)
        do i = 1, n
            do pos = post_tag_start(i), post_tag_start(i + 1) - 1
                id = post_tag_ids(pos)
                tag_posts(cursor(id)) = i
                cursor(id) = cursor(id) + 1
            end do
        end do
        deallocate(cursor)
    end subroutine build_index

    subroutine compute(posts, post_tag_start, post_tag_ids, tag_start, tag_posts, &
                       rel_idx, rel_filled)
        type(post_type), intent(in) :: posts(:)
        integer, intent(in) :: post_tag_start(:)
        integer, intent(in) :: post_tag_ids(:)
        integer, intent(in) :: tag_start(:)
        integer, intent(in) :: tag_posts(:)
        integer, allocatable, intent(out) :: rel_idx(:, :)
        integer, allocatable, intent(out) :: rel_filled(:)

        integer, allocatable :: counts(:)
        integer :: top_idx(5), top_count(5)
        integer :: n, i, p, k, j, id, c, pos, filled, min_tags

        n = size(posts)
        allocate(counts(n))
        allocate(rel_idx(5, n))
        allocate(rel_filled(n))

        do i = 1, n
            counts = 0

            do p = post_tag_start(i), post_tag_start(i + 1) - 1
                id = post_tag_ids(p)
                do k = tag_start(id), tag_start(id + 1) - 1
                    j = tag_posts(k)
                    counts(j) = counts(j) + 1
                end do
            end do

            counts(i) = 0

            top_idx = 0
            top_count = 0
            filled = 0
            min_tags = 0

            do j = 1, n
                c = counts(j)
                if (c > min_tags) then
                    pos = 5
                    do
                        if (pos < 1) exit
                        if (c <= top_count(pos)) exit
                        pos = pos - 1
                    end do
                    pos = pos + 1

                    do k = 5, pos + 1, -1
                        top_count(k) = top_count(k - 1)
                        top_idx(k) = top_idx(k - 1)
                    end do
                    top_count(pos) = c
                    top_idx(pos) = j

                    if (filled < 5) filled = filled + 1
                    if (filled == 5) then
                        min_tags = top_count(5)
                    else
                        min_tags = 0
                    end if
                end if
            end do

            rel_filled(i) = filled
            do k = 1, filled
                rel_idx(k, i) = top_idx(k)
            end do
        end do
    end subroutine compute

    function json_escape(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i, p, n, c

        n = len(s)
        allocate(character(len=6 * n + 2) :: out)
        p = 1
        out(p:p) = '"'
        p = p + 1

        do i = 1, n
            c = iachar(s(i:i))
            select case (c)
            case (34)
                out(p:p + 1) = '\"'
                p = p + 2
            case (92)
                out(p:p + 1) = '\\'
                p = p + 2
            case (8)
                out(p:p + 1) = '\b'
                p = p + 2
            case (9)
                out(p:p + 1) = '\t'
                p = p + 2
            case (10)
                out(p:p + 1) = '\n'
                p = p + 2
            case (12)
                out(p:p + 1) = '\f'
                p = p + 2
            case (13)
                out(p:p + 1) = '\r'
                p = p + 2
            case (0:7, 11, 14:31)
                write(out(p:p + 5), '(A,I2.2)') '\u00', c
                p = p + 6
            case default
                out(p:p) = s(i:i)
                p = p + 1
            end select
        end do

        out(p:p) = '"'
        p = p + 1
        out = out(1:p - 1)
    end function json_escape

    function tags_json(p) result(s)
        type(post_type), intent(in) :: p
        character(len=:), allocatable :: s
        integer :: k

        s = '['
        do k = 1, size(p%tags)
            if (k > 1) s = s // ','
            s = s // json_escape(p%tags(k)%name)
        end do
        s = s // ']'
    end function tags_json

    function post_json(p) result(s)
        type(post_type), intent(in) :: p
        character(len=:), allocatable :: s

        s = '{"_id":' // json_escape(p%id) // ',"title":' // json_escape(p%title) // &
            ',"tags":' // tags_json(p) // '}'
    end function post_json

    subroutine write_output(path, posts, rel_idx, rel_filled)
        character(len=*), intent(in) :: path
        type(post_type), intent(in) :: posts(:)
        integer, intent(in) :: rel_idx(:, :)
        integer, intent(in) :: rel_filled(:)

        integer :: u, i, k, j
        character(len=:), allocatable :: line

        open(newunit=u, file=path, status='replace', action='write')
        write(u, '(A)', advance='no') '['

        do i = 1, size(posts)
            if (i > 1) write(u, '(A)', advance='no') ','

            line = '{"_id":' // json_escape(posts(i)%id) // ',"title":' // &
                   json_escape(posts(i)%title) // ',"tags":' // tags_json(posts(i)) // &
                   ',"related":['
            do k = 1, rel_filled(i)
                if (k > 1) line = line // ','
                j = rel_idx(k, i)
                line = line // post_json(posts(j))
            end do
            line = line // ']}'
            write(u, '(A)') line
        end do

        write(u, '(A)') ']'
        close(u)
    end subroutine write_output

end module related_mod


program related_posts
    use, intrinsic :: iso_fortran_env, only: int64
    use related_mod
    implicit none

    type(post_type), allocatable :: posts(:)
    type(tag_type), allocatable :: unique(:)
    integer, allocatable :: post_tag_start(:), post_tag_ids(:)
    integer, allocatable :: tag_start(:), tag_posts(:)
    integer, allocatable :: rel_idx(:, :), rel_filled(:)
    integer :: n_unique
    integer(int64) :: t0, t1, rate, elapsed_ms

    call read_posts('../posts.json', posts)
    call build_index(posts, unique, n_unique, post_tag_start, post_tag_ids, &
                     tag_start, tag_posts)

    call system_clock(t0, rate)
    call compute(posts, post_tag_start, post_tag_ids, tag_start, tag_posts, &
                 rel_idx, rel_filled)
    call system_clock(t1)

    elapsed_ms = (t1 - t0) * 1000_int64 / rate
    write (*, '(A,I0,A)') 'Processing time (w/o IO): ', elapsed_ms, 'ms'

    call write_output('../related_posts_fortran.json', posts, rel_idx, rel_filled)
end program related_posts
