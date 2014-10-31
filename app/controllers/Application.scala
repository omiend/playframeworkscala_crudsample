package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._

import anorm._
import models._
import views._

/** メイン処理 */
object Application extends Controller {

  /** Parent form */
  val parentForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "name" -> nonEmptyText,
      "createDate" -> optional(date("yyyy-MM-dd")),
      "updateDate" -> optional(date("yyyy-MM-dd")))(Parent.apply)(Parent.unapply)
  )

  /** Child form */
  val childForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "name" -> nonEmptyText,
      "parentId" -> of[Long],
      "createDate" -> optional(date("yyyy-MM-dd")),
      "updateDate" -> optional(date("yyyy-MM-dd")))(Child.apply)(Child.unapply)
  )

  /** トップ画面起動 */
  def index(pageNum: Int) = Action { implicit request =>

    // Pagerを初期化
    val pager: Pager[Parent] = Pager[Parent]("list - crud_scala", pageNum, 0, Seq.empty)

    // データ取得
    val resultTuple = Parent.findFromTo(pager.pageNum * pager.maxListCount - pager.maxListCount, pager.maxListCount)

    // データリスト
    pager.dataList = resultTuple._1

    // 全体件数
    pager.totalRows = resultTuple._2.toInt

    Ok(html.index(pager))
  }

  /** トップ画面起動 */
  def createParent(pageNum: Int) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Parent] = Pager[Parent]("create parent - crud_scala", pageNum, 0, Seq.empty)
    // Parent作成画面に遷移
    Ok(html.createParent(pager, parentForm))
  }

  def insertParent(pageNum: Int) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Parent] = Pager[Parent]("insert - crud_scala", pageNum, 0, Seq.empty)

    // a
    parentForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(html.createParent(pager, formWithErrors))
      },
      parent => {
        // 現在日付作成（timestamp）
        def date(str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd hh:MM:dd:ss.000").parse(str)
        def nowDate: java.util.Date = new java.util.Date
        parent.createDate = Some(nowDate)
        parent.updateDate = Some(nowDate)
        Parent.create(parent)
        Redirect(routes.Application.index(pageNum)).flashing("success" -> "Parent %s has been created".format(parent.name))
      })
  }

  def editParent(pageNum: Int, id: Long) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Parent] = Pager[Parent]("edit - crud_scala", pageNum, 0, Seq.empty)
    Parent.findById(id).map { parent =>
      Ok(html.editParent(pager, id, parentForm.fill(parent)))
    }.getOrElse(NotFound)
  }

  def updateParent(pageNum: Int, id: Long) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Parent] = Pager[Parent]("update - crud_scala", pageNum, 0, Seq.empty)
    parentForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.editParent(pager, id, formWithErrors)),
      parent => {
        def date(str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd hh:MM:dd:ss.000").parse(str)
        def nowDate: java.util.Date = new java.util.Date
        parent.updateDate = Some(nowDate)
        Parent.update(id, parent)
        Redirect(routes.Application.index(pageNum)).flashing("success" -> "Parent %s has been updated".format(parent.name))
      })
  }

  def deleteParent(pageNum: Int, id: Long) = Action { implicit request =>
    Parent.deleteParent(id)
    // Pagerを初期化
    val pager: Pager[Parent] = Pager[Parent]("delete - crud_scala", pageNum, 0, Seq.empty)
    Redirect(routes.Application.index(pageNum)).flashing("success" -> "Parent has been deleted")
  }

  def createChild(pageNum: Int, parentId: Long) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Child] = Pager[Child]("create child - crud_scala", pageNum, 0, Seq.empty)
    Ok(html.createChild(pager, childForm, parentId))
  }

  def insertChild(pageNum: Int, parentId: Long) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Child] = Pager[Child]("insert - crud_scala", pageNum, 0, Seq.empty)
    childForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(html.createChild(pager, formWithErrors, parentId)).flashing("error" -> "Child %s has been errorrr")
      },
      child => {
        // 現在日付作成（timestamp）
        def date(str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd hh:MM:dd:ss.000").parse(str)
        def nowDate: java.util.Date = new java.util.Date
        child.parentId = parentId
        child.createDate = Some(nowDate)
        child.updateDate = Some(nowDate)
        Child.create(child)
        Redirect(routes.Application.index(pageNum)).flashing("success" -> "Child %s has been created".format(child.name))
      })
  }

  def editChild(pageNum: Int, id: Long) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Child] = Pager[Child]("edit - crud_scala", pageNum, 0, Seq.empty)
    Child.findById(id).map { child =>
      Ok(html.editChild(pager, id, childForm.fill(child)))
    }.getOrElse(NotFound)
  }

  def updateChild(pageNum: Int, id: Long) = Action { implicit request =>
    // Pagerを初期化
    val pager: Pager[Child] = Pager[Child]("update - crud_scala", pageNum, 0, Seq.empty)
    childForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(html.editChild(pager, id, formWithErrors)).flashing("error" -> "aaaaaaaaaaaa")
      },
      child => {
        def date(str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd hh:MM:dd:ss.000").parse(str)
        def nowDate: java.util.Date = new java.util.Date
        child.updateDate = Some(nowDate)
        Child.update(id, child)
        Redirect(routes.Application.index(pageNum)).flashing("success" -> "Child %s has been updated".format(child.name))
      })
  }

  def deleteChild(pageNum: Int, id: Long) = Action { implicit request =>
    Child.deleteChild(id)
    // Pagerを初期化
    val pager: Pager[Child] = Pager[Child]("delete - crud_scala", pageNum, 0, Seq.empty)
    Redirect(routes.Application.index(pageNum)).flashing("success" -> "Child has been deleted")
  }

  def initTestData(pInx: Int, cInx: Int) = Action { implicit request =>

    // 現在日付作成（timestamp）
    def date(str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd hh:MM:dd:ss.000").parse(str)
    def nowDate: java.util.Date = new java.util.Date

    // 全件削除
    Parent.deleteAll

    // Parent作成
    if (Parent.findAll.isEmpty) {
      for (i: Int <- 1 to pInx) {
        Parent.create(Parent(Id(i), "親名" + i, Some(nowDate), Some(nowDate)))
      }
    }

    // Child作成
    if (Child.findAll.isEmpty) {
      var childIndex: Long = 1
      for (p: Int <- 1 to pInx) {
        for (c: Int <- 1 to cInx) {
            Child.create(Child(Id(childIndex), "子名" + c, p, Some(nowDate), Some(nowDate)))
            childIndex = childIndex + 1
        }
      }
    }
    Redirect(routes.Application.index(1))
  }

}