require 'singleton'

class Calendar
include Singleton
  def initialize
    @@date = 1
  end

  def get_date
    @@date
  end

  def advance
    @@date = @@date + 1
    nil
  end
end

class Book
  def initialize(id, title, author)
    @id = id
    @title = title
    @author = author
    @due_date = nil
  end

  def get_id
    @id
  end

  def get_title
    @title
  end

  def get_author
    @author
  end

  def get_due_date
    @due_date
  end

  def check_out(due_date)
    @due_date = due_date
    nil
  end

  def check_in
    @due_date = nil
  end

  def to_s
    "#{@id}: #{@title}, by #{@author}"
  end
end

class Member
  def initialize(name, library)
    @name = name
    @library = library
    @books = []
  end

  def get_name
    @name
  end

  def get_library
    @library
  end

  def check_out(book)
    @books.push(book)
    nil
  end

  def give_back(book)
    @books.delete(book)
  end

  def get_books
    @books
  end

  def send_overdue_notice(notice)
    "#{@name}:\n#{notice}"
  end
end

class Library
  include Singleton
  def initialize
    get_books
    @calendar = Calendar.instance
    @members = []
    @isopen = false
    @currentmember = nil
  end

  def open
    if @isopen == true
      raise 'The library is already open!'
    end
    @isopen = true
    "Today is day #{@calendar.get_date}"
  end

  def find_all_overdue_books
    result = ''
    @members.each { |x| result.concat(send_overdue_notice(x)) if send_overdue_notice(x) != 'None' }
    if result == ''
      return 'None'
    end
    result
  end

  def issue_card(name_of_member)
    check_library_open

    if find_member(name_of_member) != nil
      "#{name_of_member} already has a library card."
    else
      @members.push(Member.new(name_of_member, self))
      "Library card issued to #{name_of_member}."
    end
  end

  def serve(name_of_member)
    check_library_open

    member = find_member(name_of_member)
    if (member == nil)
      return "#{name_of_member} does not have a library card."
    end
    @currentmember = member
    "Now serving #{name_of_member}."
  end

  def find_overdue_books
    check_library_open
    check_current_member
    send_overdue_notice(@currentmember)
  end

  def check_in(*book_numbers)
    check_library_open
    check_current_member
    book_numbers.each{ |x| raise "The member does not have book #{x}." if find_book(@currentmember.get_books, x) == nil }
    book_numbers.each{ |x| check_in_book(find_book(@currentmember.get_books, x)) }
    "#{@currentmember.get_name} has returned #{book_numbers.length} books"
  end

  def check_out(*book_numbers)
    check_library_open
    check_current_member
    book_numbers.each{ |x| raise "The library does not have book #{x}." if find_book(@books, x) == nil }
    book_numbers.each{ |x| check_out_book(find_book(@books, x)) }
    "#{book_numbers.length} books has been checked out to #{@currentmember.get_name}"
  end

  def renew(*book_numbers)
    check_library_open
    check_current_member
    book_numbers.each{ |x| raise "The member does not have book #{x}." if find_book(@currentmember.get_books, x) == nil }
    book_numbers.each{ |x| renew_book(find_book(@currentmember.get_books, x)) }
    "#{book_numbers.length} books has been renewed for #{@currentmember.get_name}"
  end

  def search(string)
    if string.length < 4
      return 'Search string must contain at least four characters.'
    end

    books = ''
    result = @books.uniq{|x| x.get_title + x.get_author}.find_all{ |x| x.get_title.downcase.index(string.downcase) != nil or x.get_author.downcase.index(string.downcase) }
    if (result.length == 0)
      return 'No books found.'
    else
      result.each{|x| books.concat("#{x.to_s}\n")}
    end
    books
  end

  def close
    check_library_open
    @isopen = false
    'Good night'
  end

  def quit
    @isopen = false
    'The library is now closed for reservations.'
  end

# Idea of reading file from Michael Williams 12/19/2005, example 1
# Licensed under Create Commons Attribution License
  def get_books
    file = File.new("collection.txt", "r")
    id = 1
    @books = []
    while (line = file.gets)
      parsedstrings = line.split(',')
      if (parsedstrings.length == 2)
        @books.push(Book.new(id, parsedstrings[0].strip, parsedstrings[1].strip))
      end
      id = id + 1
    end
    file.close
  end

  def send_overdue_notice(member)
    result = ''
    member.get_books.each{ |x| result.concat("#{x.to_s}\n") if x.get_due_date <= @calendar.get_date}
    if (result != '')
      return member.send_overdue_notice(result)
    end
    return 'None'
  end

  def check_out_book(book)
    book.check_out(@calendar.get_date + 7)
    @currentmember.check_out(book)
    @books.delete(book)
  end

  def renew_book(book)
    book.check_out(@calendar.get_date + 7)
    @currentmember.check_out(book)
  end

  def check_in_book(book)
    book.check_in
    @currentmember.give_back(book)
    @books.push(book)
  end

  def find_member(name_of_member)
    @members.find{ |x| x.get_name == name_of_member and x.get_library.equal?(self)}
  end

  def find_book(books, id)
    books.find{|y| y.get_id == id }
  end

  def check_library_open
    if @isopen == false
      raise 'The library is not open.'
    end
  end

  def check_current_member
    if @currentmember == nil
      raise 'No member is currently being served.'
    end
  end

  private :get_books, :find_member, :check_library_open, :check_current_member, :send_overdue_notice, :find_book, :check_in_book, :check_out_book, :renew_book
end