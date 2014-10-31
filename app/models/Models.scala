package models

import java.util.Date
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.reflect.BeanProperty

case class Parent(id: Pk[Long] = NotAssigned, name: String, var createDate: Option[Date], var updateDate: Option[Date]) {
  var childList: Seq[Child] = Seq.empty
}
case class Child(id: Pk[Long] = NotAssigned, name: String, var parentId: Long, var createDate: Option[Date], var updateDate: Option[Date])

object Parent {

  // -- Parsers
  /**
   * Parse a Parent from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("parent.id") ~
    get[String]("parent.name") ~
    get[Date]("parent.create_date") ~
    get[Date]("parent.update_date") map {
      case id~name~createDate~updateDate => Parent(id, name, Option(createDate), Option(updateDate))
    }
  }

  // -- Queries

  /**
   * Retrieve a child from the id.
   */
  def findById(id: Long): Option[Parent] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        select *
          from parent
         where id = {id}
        """
      ).on(
        'id -> id
      ).as(
        Parent.simple.singleOpt
      )
    }
  }

  /**
   * Retrieve a child from the id.
   */
  def findAll: Seq[Parent] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        select *
          from parent
        """
      ).as(
        Parent.simple *
      )
    }
  }

  /**
   * Retrieve a child from the id.
   */
  def findFromTo(offset: Int, maxPageCount: Int) = {
    DB.withConnection { implicit connection =>

      // 親テーブル取得
      val parentList: Seq[Parent] = SQL(
        """
        select *
          from parent
          limit {maxPageCount} offset {offset}
        """
      ).on(
        'offset -> offset,
        'maxPageCount -> maxPageCount
      ).as(
        Parent.simple *
      )

      // 子テーブル取得
      val childSetList: Seq[(Child, Parent)] = Child.findAllWithParent

      // 返却用リスト
      var returnList: Seq[Parent] = Seq.empty

      // 親テーブルに子テーブルを結合
      for (parent: Parent <- parentList) {
        for (childSet <- childSetList if childSet._2.id == parent.id) { // idでフィルター
          val child: Child = childSet._1
          val parent2: Parent = childSet._2
          parent.childList = parent.childList :+ child
        }
        returnList = returnList :+ parent
      }

      // 件数取得
      val totalRows = SQL(
        """
        select count(*)
          from parent
        """
      ).as(scalar[Long].single)

      (returnList, totalRows)
    }

  }

  /**
   * Create a User.
   */
  def create(parent: Parent): Parent = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into parent values (
              {id}
             ,{name}
             ,{create_date}
             ,{update_date}
          )
        """
      ).on(
        'id -> parent.id,
        'name -> parent.name,
        'create_date -> parent.createDate,
        'update_date -> parent.updateDate
      ).executeUpdate()
      parent
    }
  }

  def update(id: Long, parent: Parent) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update parent
          set name = {name}
          ,update_date = {updateDate}
          where id = {id}
        """
      ).on(
        'id -> id,
        'name -> parent.name,
        'updateDate -> parent.updateDate
      ).executeUpdate()
    }
  }

  def deleteParent(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL("delete from child where parent_id = {id}").on('id -> id).executeUpdate()
      SQL("delete from parent where id = {id}").on('id -> id).executeUpdate()
    }
  }

  def deleteAll = {
    DB.withConnection { implicit connection =>
      SQL("delete from child").executeUpdate()
      SQL("delete from parent").executeUpdate()
    }
  }

}

object Child {

  // -- Parsers
  /**
   * Parse a Child from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("child.id") ~
    get[String]("child.name")  ~
    get[Long]("child.parent_id") ~
    get[Date]("child.create_date") ~
    get[Date]("child.update_date") map {
      case id~name~parentId~createDate~updateDate => Child(id, name, parentId, Option(createDate), Option(updateDate))
    }
  }

  // -- Queries

  /**
   * Retrieve a child from the id.
   */
  def findAll: Seq[Child] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        select *
          from child
        """
      ).as(
        Child.simple *
      )
    }
  }

  /**
   * Retrieve a child from the id.
   */
  def findById(id: Long): Option[Child] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        select *
          from child
         where id = {id}
        """
      ).on(
        'id -> id
      ).as(
        Child.simple.singleOpt
      )
    }
  }

  /**
   * Create a User.
   */
  def create(child: Child): Child = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into child values (
             {id}
            ,{name}
            ,{parent_id}
            ,{create_date}
            ,{update_date}
          )
        """
      ).on(
        'id -> child.id,
        'name -> child.name,
        'parent_id -> child.parentId,
        'create_date -> child.createDate,
        'update_date -> child.updateDate
      ).executeUpdate()
      child
    }
  }

  def update(id: Long, child: Child) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update child
          set name = {name}
          ,update_date = {updateDate}
          where id = {id}
        """
      ).on(
        'id -> id,
        'name -> child.name,
        'updateDate -> child.updateDate
      ).executeUpdate()
    }
  }

  def deleteChild(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL("delete from child where id = {id}").on('id -> id).executeUpdate()
    }
  }

  /**
   * Parse a (Child,Parent) from a ResultSet
   */
  val withParent = {
    Child.simple ~
    Parent.simple map {
      case child~parent => (child,parent)
    }
  }

  /**
   * Retrieve a child from the id.
   */
  def findAllWithParent: Seq[(Child, Parent)] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          select *
            from child
            join parent
              on parent.id = child.parent_id;
        """
      ).as(
        Child.withParent *
      )
    }
  }
}
