require 'test/unit'

include Test::Unit::Assertions


class Library_test

  def initialize
    @@library = Library.instance
    @@library_open = false
    @@issued_user1 = false
    @@served_user1 = false
    @@calendar = Calendar.instance
  end

  def test_all
    test_open
    test_search
    test_issue_card
    test_serve
    test_check_out_in
    test_overdue
    test_renew
    test_close
    test_quit
  end

  def test_open
    if (@@library_open)
      begin
        @@library.open
        assert(false, 'an exception should have occurred')
      rescue RuntimeError => e
        assert(e.message == 'The library is already open!', 'invalid exception message')
      end
    else
      msg = @@library.open
      assert(msg == "Today is day #{@@calendar.get_date}", 'Unexpected message from Library.open')
      @@library_open = true
    end
    true
  end

  def test_issue_card
    test_close

    begin
      msg = @@library.issue_card('testuser')
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The library is not open.', 'invalid exception message')
    end

    test_open
    if (@@issued_user1 == false)
      msg = @@library.issue_card('testuser')
      assert(msg == 'Library card issued to testuser.', 'Unexpected message from Library.issue_card')
      @@issued_user1 = true
    end

    msg = @@library.issue_card('testuser')
    assert(msg == 'testuser already has a library card.', 'Unexpected message from Library.issue_card')
  end

  def test_serve
    test_close

    begin
      msg = @@library.serve('testuser')
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The library is not open.', 'invalid exception message')
    end

    test_open

    msg = @@library.serve('nonexistinguser')
    assert(msg == 'nonexistinguser does not have a library card.', 'Unexpected message from Library.serve')

    test_issue_card

    msg = @@library.serve('testuser')
    assert(msg == 'Now serving testuser.', 'Unexpected message from Library.serve')

    @@served_user1 = true
  end

  def test_check_out_in
    test_close

    begin
      msg = @@library.check_out(1,2)
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The library is not open.', 'invalid exception message')
    end

    begin
      msg = @@library.check_in(1,2)
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The library is not open.', 'invalid exception message')
    end

    test_open

    if (@@served_user1 == false)
      begin
        msg = @@library.check_out(1,2)
        assert(false, 'an exception should have occurred')
      rescue RuntimeError => e
        assert(e.message == 'No member is currently being served.', 'invalid exception message')
      end

      begin
        msg = @@library.check_in(1,2)
        assert(false, 'an exception should have occurred')
      rescue RuntimeError => e
        assert(e.message == 'No member is currently being served.', 'invalid exception message')
      end
    end

    test_serve

    msg = @@library.check_out(1,2)
    assert(msg == '2 books has been checked out to testuser', 'Unexpected message from Library.check_out')

    begin
      msg = @@library.check_out(1,2)
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The library does not have book 1.', 'invalid exception message')
    end

    msg = @@library.check_in(1,2)
    assert(msg == 'testuser has returned 2 books', 'Unexpected message from Library.check_in')

    begin
      msg = @@library.check_in(1,2)
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The member does not have book 1.', 'invalid exception message')
    end


    msg = @@library.check_out(1,2)
    assert(msg == '2 books has been checked out to testuser', 'Unexpected message from Library.check_out')
    msg = @@library.check_in(1,2)
    assert(msg == 'testuser has returned 2 books', 'Unexpected message from Library.check_in')
  end

  def test_overdue
    msg = @@library.find_all_overdue_books
    assert(msg == 'None', 'Unexpected message from Library.find_all_overdue_books')

    test_open
    test_serve

    @@library.check_out(1,2)

    msg = @@library.find_all_overdue_books
    assert(msg == 'None', 'Unexpected message from Library.find_all_overdue_books')

    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance

    msg = @@library.find_all_overdue_books
    assert(msg == "testuser:\n1: book1, by author1\n2: book2, by author2\n", 'Unexpected message from Library.find_all_overdue_books')

    msg = @@library.find_overdue_books
    assert(msg == "testuser:\n1: book1, by author1\n2: book2, by author2\n", 'Unexpected message from Library.find_overdue_books')

    msg = @@library.check_in(1,2)
    assert(msg == 'testuser has returned 2 books', 'Unexpected message from Library.check_in')

    msg = @@library.find_overdue_books
    assert(msg == 'None', 'Unexpected message from Library.find_overdue_books')

    msg = @@library.find_all_overdue_books
    assert(msg == 'None', 'Unexpected message from Library.find_all_overdue_books')
  end

  def test_renew

    test_close

    begin
      msg = @@library.renew(1,2)
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The library is not open.', 'invalid exception message')
    end

    test_open

    if (@@served_user1 == false)
      begin
        msg = @@library.renew(1,2)
        assert(false, 'an exception should have occurred')
      rescue RuntimeError => e
        assert(e.message == 'No member is currently being served.', 'invalid exception message')
      end
    end

    test_serve

    begin
      msg = @@library.renew(1,2)
      assert(false, 'an exception should have occurred')
    rescue RuntimeError => e
      assert(e.message == 'The member does not have book 1.', 'invalid exception message')
    end

    @@library.check_out(1,2)

    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance
    @@calendar.advance

    msg = @@library.find_overdue_books
    assert(msg == "testuser:\n1: book1, by author1\n2: book2, by author2\n", 'Unexpected message from Library.find_overdue_books')

    msg = @@library.renew(1,2)
    assert(msg == '2 books has been renewed for testuser', 'Unexpected message from Library.find_overdue_books')

    msg = @@library.check_in(1,2)
    assert(msg == 'testuser has returned 2 books', 'Unexpected message from Library.check_in')
  end

  def test_search
    msg = @@library.search('not')
    assert(msg == 'Search string must contain at least four characters.', 'Unexpected message from Library.search')
    msg = @@library.search('nothing')
    assert(msg == 'No books found.', 'Unexpected message from Library.search')
    msg = @@library.search('book')
    assert(msg.include?("1: book1, by author1"), 'Unexpected message from Library.search')
    assert(msg.include?("2: book2, by author2"), 'Unexpected message from Library.search')
    assert(msg.include?("3: book3, by author3"), 'Unexpected message from Library.search')
    assert(msg.include?("4: book4, by author4"), 'Unexpected message from Library.search')
    assert(msg.include?("5: book5, by author5"), 'Unexpected message from Library.search')
    msg = @@library.search('author')
    assert(msg.include?("1: book1, by author1"), 'Unexpected message from Library.search')
    assert(msg.include?("2: book2, by author2"), 'Unexpected message from Library.search')
    assert(msg.include?("3: book3, by author3"), 'Unexpected message from Library.search')
    assert(msg.include?("4: book4, by author4"), 'Unexpected message from Library.search')
    assert(msg.include?("5: book5, by author5"), 'Unexpected message from Library.search')
  end

  def test_close
    if (@@library_open)
      msg = @@library.close
      assert(msg == 'Good night', 'Unexpected message from Library.close')
      @@library_open = false
    else
      begin
        @@library.close
        assert(false, 'an exception should have occurred')
      rescue RuntimeError => e
        assert(e.message == 'The library is not open.', 'invalid exception message')
      end
    end
    true
  end

  def test_quit
    msg = @@library.quit
    assert(msg == 'The library is now closed for reservations.', 'Unexpected message from Library.quit')
    @@library_open = false
    true
  end

end