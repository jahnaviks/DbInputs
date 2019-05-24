CREATE OR REPLACE PACKAGE appsecurity_new IS
/***********************************************************************************************************************
 * Package Specification  : appsecurity_new
 *
 * Purpose: This Package contains procedures related to the security module
 *
 * MODIFICATION HISTORY
 * Person      Date        Version  Comments
 * ---------   ----------  -------  ------------------------------------
 * Satish       04/04/2004   1.0    Original
 * Suhail M.  11/03/2006   2.0    Copied original and changed table references keep package in sync
 *            with new security tables with "_new" suffix.  Also, added t_generic_cursor.
 *
 **********************************************************************************************************************/
  TYPE t_generic_cursor IS REF CURSOR;

  TYPE t_BinaryRec IS TABLE OF NUMBER(1)
    INDEX BY BINARY_INTEGER;

  TYPE t_IdRec IS TABLE OF NUMBER(10)
    INDEX BY BINARY_INTEGER;

  TYPE t_objprivilege IS RECORD(
    object_id                                   NUMBER(10),
    access_rights                               NUMBER(3)
  );

  TYPE t_objprivileges IS TABLE OF t_objprivilege
    INDEX BY BINARY_INTEGER;

  TYPE t_userrecord IS RECORD(
    user_id                                     sec_user_new.user_id%TYPE,
    user_code                                   sec_user_new.user_code%TYPE,
    first_name                                  sec_user_new.first_name%TYPE,
    last_name                                   sec_user_new.last_name%TYPE,
    effective_date                              sec_user_new.effective_date%TYPE,
    end_date                                    sec_user_new.end_date%TYPE,
    is_active                                   sec_user_new.is_active%TYPE
  );   --t_userrecord

  TYPE t_users IS TABLE OF t_userrecord
    INDEX BY BINARY_INTEGER;

  TYPE t_grouprecord IS RECORD(
    GROUP_ID                                    sec_group_new.user_group_id%TYPE,
    group_name                                  sec_group_new.user_group_name%TYPE,
    group_desc                                  sec_group_new.user_group_desc%TYPE
  );   --t_grouprecord

  TYPE t_groups IS TABLE OF t_grouprecord
    INDEX BY BINARY_INTEGER;

  TYPE t_objectrecord IS RECORD(
    object_id                                   sec_object_new.object_id%TYPE,
    object_name                                 sec_object_new.object_name%TYPE,
    parent_id                                   sec_object_new.parent_object_id%TYPE,
    prog_name                                   sec_object_new.object_prog_name%TYPE,
    object_type_id                              sec_object_new.object_type_id%TYPE,
    object_desc                                 sec_object_new.object_desc%TYPE,
    object_text                                 sec_object_new.object_text%TYPE,
    object_icon_key                             sec_object_new.object_icon_key%TYPE,
    assembly_name                               sec_object_new.assembly_name%TYPE,
    display_order                               sec_object_new.display_order%TYPE,
    active_ir                                   sec_object_new.is_active%TYPE
  );   --t_objectrecord

  TYPE t_objects IS TABLE OF t_objectrecord
    INDEX BY BINARY_INTEGER;

  PROCEDURE getusers(
    p_rc_users                         OUT    scribe_pkg.t_generic_cursor);

  PROCEDURE getgroups(
    p_rc_groups                        OUT    scribe_pkg.t_generic_cursor);

  PROCEDURE getuserdetails(
    p_user_id                          IN     NUMBER,
    p_rc_user_details                  OUT    scribe_pkg.t_generic_cursor,
    p_rc_groups_assigned               OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor);

  PROCEDURE getusers(
    p_rc_users                         OUT    scribe_pkg.t_generic_cursor,
    p_rc_groups_assigned               OUT    scribe_pkg.t_generic_cursor);

  PROCEDURE getgroupdetails(
    p_user_group_id                    IN     NUMBER,
    p_rc_group                         OUT    scribe_pkg.t_generic_cursor,
    p_rc_member_users                  OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor);   --getgroups

  PROCEDURE GetObjects(
    p_rc_objects                       OUT    scribe_pkg.t_generic_cursor);

  PROCEDURE GetObject(
    p_object_id                        IN     NUMBER,
    p_rc_objects                       OUT    scribe_pkg.t_generic_cursor,
    p_rc_object_groups                 OUT    scribe_pkg.t_generic_cursor);   --getobjects

  PROCEDURE getobjecttypes(
    p_rc_objecttypes                   OUT    scribe_pkg.t_generic_cursor);

  PROCEDURE saveusers(
    p_users                            IN     VARCHAR2,
    p_result                           OUT    VARCHAR2);

  --saveusers
  PROCEDURE saveuser(
    p_user_id                          IN OUT NUMBER,
    p_user_code                        IN     VARCHAR2,
    p_first_name                       IN     VARCHAR2,
    p_last_name                        IN     VARCHAR2,
    p_effective_date                   IN     DATE,
    p_end_date                         IN     DATE,
    p_is_active                        IN     NUMBER,
    p_dept_code_lk                     IN     NUMBER,
    p_user_title_lk                    IN     NUMBER,
    p_office_code_lk                   IN     NUMBER,
    p_business_area_lk                 IN     NUMBER,
    p_group_assigned                   IN     VARCHAR2,
    p_change_user                      IN     VARCHAR2,
    p_error_message                    OUT    VARCHAR2);   --SaveUser

  PROCEDURE savegroupassignment(
    p_user_id                          IN     NUMBER,
    p_group_assigned                   IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_change_user                      IN     VARCHAR2,
    p_result                           OUT    VARCHAR2);   --SaveGroupAssignment

  PROCEDURE SaveGroup(
    p_group_id                         IN OUT NUMBER,
    p_group_name                       IN     VARCHAR2,
    p_group_desc                       IN     VARCHAR2,
    p_members                          IN     SCRIBE_PKG.t_IdRec,
    p_object_assigned                  IN     SCRIBE_PKG.t_IdRec,
    p_object_rights                    IN     SCRIBE_PKG.t_IdRec,
    p_rc_group                         OUT    scribe_pkg.t_generic_cursor,
    p_rc_member_users                  OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor,
    p_change_user                      IN     VARCHAR2,
    p_error_message                    IN OUT VARCHAR2);   --SaveGroup

  PROCEDURE savegroupmembers(
    p_group_id                         IN     NUMBER,
    p_members                          IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_result                           OUT    VARCHAR2);   --savegroupmembers

  PROCEDURE savegroupobjectassignment(
    p_group_id                         IN     NUMBER,
    p_object_assigned                  IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_result                           OUT    VARCHAR2);   --savegroupobjectassignment

  PROCEDURE getobjectprivsrec(
    p_data_set                         IN     VARCHAR2,
    p_data_list                        OUT    t_objprivileges,
    p_result                           OUT    VARCHAR2);   --GetObjectPrivsRec

  PROCEDURE saveobject(
    p_object_id                        IN OUT NUMBER,
    p_object_name                      IN     VARCHAR2,
    p_parent_id                        IN     NUMBER,
    p_prog_name                        IN     VARCHAR2,
    p_object_type_id                   IN     NUMBER,
    p_object_desc                      IN     VARCHAR2,
    p_object_text                      IN     VARCHAR2,
    p_object_icon_key                  IN     VARCHAR2,
    p_assembly_name                    IN     VARCHAR2,
    p_display_order                    IN     NUMBER,
    p_is_active                        IN     NUMBER,
    p_is_node                          IN     NUMBER,
    p_help_topic_id                    IN     NUMBER,
    p_group_id_assigned                IN     scribe_pkg.t_IdRec,
    p_group_rights                     IN     scribe_pkg.t_IdRec,
    p_rc_objects                       OUT    scribe_pkg.t_generic_cursor,
    p_rc_object_groups                 OUT    scribe_pkg.t_generic_cursor,
    p_change_user                      IN     VARCHAR2,
    p_result                           OUT    VARCHAR2);   --SaveObject

  PROCEDURE saveobjectgroupassignment(
    p_object_id                        IN     NUMBER,
    p_group_assigned                   IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_result                           OUT    VARCHAR2);   --SaveObjectGroupAssignment

  FUNCTION IsUserInGroup(
    p_user_code                        IN     VARCHAR2,
    p_group_name                       IN     VARCHAR2)
    RETURN NUMBER;

  PROCEDURE getgroupsassigned(
    p_user_code                        IN     VARCHAR,
    p_rc_groups                        OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor);   --GetGroupsAssigned

  PROCEDURE deleteuser(
    p_user_id                          IN     NUMBER,
    p_result                           OUT    VARCHAR2);

  PROCEDURE deletegroup(
    p_group_id                         IN     NUMBER,
    p_result                           OUT    VARCHAR2);

  PROCEDURE deleteobject(
    p_object_id                        IN     NUMBER,
    p_result                           OUT    VARCHAR2);

  PROCEDURE GetObjectsAccessRightsForUser(
    p_user_code                        IN     VARCHAR2,
    p_rc                               OUT    SCRIBE_PKG.t_generic_cursor,
    p_error_message                    IN OUT VARCHAR2);

  PROCEDURE GetBBObjectGroup(
    p_object_name                      IN     VARCHAR2,
    p_rc                               OUT    SCRIBE_PKG.t_generic_cursor,
    p_error_message                    IN OUT VARCHAR2);

  FUNCTION UserHasCreateForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER;

  FUNCTION UserHasReadForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER;

  FUNCTION UserHasUpdateForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER;
    
    FUNCTION UserHasUpdateOnObject(
    p_user_code                        IN     VARCHAR2,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER;

  FUNCTION UserHasDeleteForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER;

  FUNCTION ObjectHasCreateForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER;

  FUNCTION ObjectHasReadForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER;

  FUNCTION ObjectHasUpdateForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER;

  FUNCTION ObjectHasDeleteForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER;

  FUNCTION GetObjectParentHierarchyName(
    p_object_id                        IN     NUMBER)
    RETURN VARCHAR2;

  FUNCTION GetObjectSortLevel(
    p_object_id                        IN     NUMBER)
    RETURN NUMBER;
END;   -- Package spec appsecurity_new

/
CREATE OR REPLACE PACKAGE BODY appsecurity_new IS
--------------------------------------------------------------------------------
--Forward Declarations of Private Procedures
--------------------------------------------------------------------------------
  PROCEDURE BlobToBinary(
    p_blob                             IN     VARCHAR2,
    p_result                           OUT    VARCHAR2,
    p_ListLen                          OUT    NUMBER,
    p_BinaryList                       OUT    t_BinaryRec);

  PROCEDURE BlobToBinary(
    p_blob                             IN     VARCHAR2,
    p_result                           OUT    VARCHAR2,
    p_ListLen                          OUT    NUMBER,
    p_BinaryList                       OUT    t_IdRec);

  FUNCTION UserIdFromCode(
    p_user_code                        IN     VARCHAR2)
    RETURN NUMBER;

--------------------------------------------------------------------------------
-- Purpose:   Get all users profile
--
-- Usage:     getusers
--
--------------------------------------------------------------------------------
  PROCEDURE getusers(
    p_rc_users                         OUT    scribe_pkg.t_generic_cursor,
    p_rc_groups_assigned               OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_users FOR
      SELECT   su.user_id, su.user_code, su.first_name, su.last_name, su.effective_date, su.end_date, su.is_active,
               su.created_by, su.created_date, su.updated_by, su.updated_date
          FROM sec_user_new su
      ORDER BY su.user_code;

    --Get the groups assigned to each userid
    OPEN p_rc_groups_assigned FOR
      SELECT   ga.user_id, ga.user_group_id, g.user_group_name, g.user_group_desc
          FROM sec_group_new g, sec_group_assignment_new ga
         WHERE g.user_group_id = ga.user_group_id
      ORDER BY g.user_group_name;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getusers;

--------------------------------------------------------------------------------
-- Purpose:   Get all users
--
-- Usage:     getusers
--
--------------------------------------------------------------------------------
  PROCEDURE getusers(
    p_rc_users                         OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_users FOR
      SELECT   su.user_id, su.user_code, su.first_name, su.last_name, su.effective_date, su.end_date, su.is_active,
               dept_code_lk, user_title_lk, office_code_lk, (SELECT LOOKUP_NAME
                                                               FROM gen_lookup_item
                                                              WHERE LOOKUP_ID = dept_code_lk) AS dept_name,
                      business_area_lk, (SELECT LOOKUP_NAME
                                                               FROM gen_lookup_item
                                                              WHERE LOOKUP_ID = business_area_lk) AS business_area_name,
                (SELECT LOOKUP_NAME
                  FROM gen_lookup_item
                 WHERE LOOKUP_ID = user_title_lk) AS user_title, (SELECT LOOKUP_NAME
                                                                    FROM gen_lookup_item
                                                                   WHERE LOOKUP_ID = office_code_lk) AS office_name,
               su.created_by, su.created_date, su.updated_by, su.updated_date
          FROM sec_user_new su, gen_user_attributes gu
         WHERE su.user_id = gu.user_id(+)
      ORDER BY su.user_code;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getusers;

   --------------------------------------------------------------------------------
-- Purpose:   Get all users
--
-- Usage:     getusers
--
--------------------------------------------------------------------------------
  PROCEDURE getgroups(
    p_rc_groups                        OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_groups FOR
      SELECT   user_group_id, user_group_name, user_group_desc
          FROM sec_group_new
      ORDER BY user_group_name;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getgroups;

--------------------------------------------------------------------------------
-- Purpose:   Get all users
--
-- Usage:     getusers
--
--------------------------------------------------------------------------------
  PROCEDURE getuserdetails(
    p_user_id                          IN     NUMBER,
    p_rc_user_details                  OUT    scribe_pkg.t_generic_cursor,
    p_rc_groups_assigned               OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_user_details FOR
      SELECT   usr.user_id, usr.user_code, usr.first_name, usr.last_name, usr.effective_date, usr.end_date,
               usr.is_active, dept_code_lk, user_title_lk, business_area_lk, office_code_lk, usr.created_by, usr.created_date,
               usr.updated_by, usr.updated_date
          FROM sec_user_new usr, gen_user_attributes gu
         WHERE usr.user_id = gu.user_id(+) AND usr.user_id = p_user_id
      ORDER BY usr.user_code;

    --Get the groups assigned to the user
    OPEN p_rc_groups_assigned FOR
      SELECT   ga.user_id, ga.user_group_id, g.user_group_name, g.user_group_desc
          FROM sec_group_new g, sec_group_assignment_new ga
         WHERE g.user_group_id = ga.user_group_id AND ga.user_id = p_user_id
      ORDER BY g.user_group_name;

    --Get the objects assigned to each group the user belongs
    OPEN p_rc_objects_assigned FOR
      SELECT     obj.object_id, obj.object_name, obj.parent_object_id, obj.object_prog_name, obj.object_desc,
                 obj.object_text, obj.object_icon_key, obj.assembly_name, obj.display_order, obj.is_active,
                 obj.is_node, NVL(obj.help_topic_id, 0) help_topic_id, objt.object_type_desc,
                 appsecurity_new.UserHasCreateForObject(p_user_id, obj.object_id) Has_Create,
                 appsecurity_new.UserHasReadForObject(p_user_id, obj.object_id) Has_Read,
                 appsecurity_new.UserHasUpdateForObject(p_user_id, obj.object_id) Has_Update,
                 appsecurity_new.UserHasDeleteForObject(p_user_id, obj.object_id) Has_Delete, LEVEL
            FROM sec_object_new obj, sec_object_type_new objt
           WHERE obj.object_type_id = objt.object_type_id
      START WITH obj.object_id = 1
      CONNECT BY PRIOR obj.object_id = obj.parent_object_id
        ORDER SIBLINGS BY obj.display_order;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getuserdetails;

--------------------------------------------------------------------------------
--
-- Purpose:   Get all groups
--
-- Usage:     getgroups
--
--------------------------------------------------------------------------------
  PROCEDURE getgroupdetails(
    p_user_group_id                    IN     NUMBER,
    p_rc_group                         OUT    scribe_pkg.t_generic_cursor,
    p_rc_member_users                  OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_group FOR
      SELECT grp.user_group_id, grp.user_group_name, grp.user_group_desc
        FROM sec_group_new grp
       WHERE grp.user_group_id = p_user_group_id;

    --Get the member users for each group
    OPEN p_rc_member_users FOR
      SELECT grpa.user_id, usr.user_code, usr.last_name, usr.first_name
        FROM sec_group_assignment_new grpa, sec_user_new usr
       WHERE grpa.user_group_id = p_user_group_id AND grpa.user_id = usr.user_id;

    --Get the objects assigned to each group
    OPEN p_rc_objects_assigned FOR
      SELECT     obj.object_id, LPAD(' ', LEVEL * 3) || obj.object_name object_name, obj.parent_object_id,
                 obj.display_order, obj.has_delete, obj.has_update, obj.has_read, obj.has_create
            FROM (SELECT   obj.object_id, obj.object_name, obj.parent_object_id, obj.display_order,
                           DECODE(SUM(BITAND(NVL(gop.access_rights, 0), 8)), 0, 0, 1) has_delete,
                           DECODE(SUM(BITAND(NVL(gop.access_rights, 0), 4)), 0, 0, 1) has_update,
                           DECODE(SUM(BITAND(NVL(gop.access_rights, 0), 2)), 0, 0, 1) has_read,
                           DECODE(SUM(BITAND(NVL(gop.access_rights, 0), 1)), 0, 0, 1) has_create
                      FROM sec_object_new obj, sec_group_object_priv_new gop
                     WHERE gop.object_id(+) = obj.object_id AND gop.user_group_id(+) = p_user_group_id
                  GROUP BY obj.object_id, obj.object_name, obj.parent_object_id, obj.display_order) obj
      START WITH obj.object_id = 1
      CONNECT BY PRIOR obj.object_id = obj.parent_object_id
        ORDER SIBLINGS BY obj.display_order;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getgroupdetails;

  PROCEDURE GetObjects(
    p_rc_objects                       OUT    scribe_pkg.t_generic_cursor) IS
  BEGIN
    OPEN p_rc_objects FOR
      SELECT     o.object_id, LPAD(' ', LEVEL * 3) || o.object_name object_name, o.parent_object_id,
                 ot.object_type_desc, o.object_text, o.object_icon_key
            FROM sec_object_new o, sec_object_type_new ot
           WHERE o.object_type_id = ot.object_type_id
      START WITH o.object_id = 1
      CONNECT BY PRIOR o.object_id = o.parent_object_id
        ORDER SIBLINGS BY display_order;
  END GetObjects;   -- end of Procedure GetObjects

--------------------------------------------------------------------------------
--
-- Purpose:   Get all objects hierarchy
--
-- Usage:     getobject
--
--------------------------------------------------------------------------------
  PROCEDURE GetObject(
    p_object_id                        IN     NUMBER,
    p_rc_objects                       OUT    scribe_pkg.t_generic_cursor,
    p_rc_object_groups                 OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_objects FOR
      SELECT o.object_id, o.object_name, o.parent_object_id, o.object_prog_name, o.object_type_id, ot.object_type_desc,
             o.object_desc, o.object_text, o.object_icon_key, o.assembly_name, o.display_order, o.is_active, o.is_node,
             o.help_topic_id, o.created_by, o.created_date, o.updated_by, o.updated_date
        FROM sec_object_new o, sec_object_type_new ot
       WHERE o.object_type_id = ot.object_type_id AND o.object_id = p_object_id;

    --Get the groups assigned to each object
    OPEN p_rc_object_groups FOR
      SELECT grp.user_group_id, grp.user_group_name, grp.user_group_desc,
             appsecurity_new.ObjectHasCreateForGroup(p_object_id, grp.user_group_id) has_create,
             appsecurity_new.ObjectHasUpdateForGroup(p_object_id, grp.user_group_id) has_update,
             appsecurity_new.ObjectHasReadForGroup(p_object_id, grp.user_group_id) has_read,
             appsecurity_new.ObjectHasDeleteForGroup(p_object_id, grp.user_group_id) has_delete
        FROM sec_group_new grp;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END GetObject;

--------------------------------------------------------------------------------
--
-- Purpose:   Get all users profile
--
-- Usage:     getusers
--
-------------------------------------------------SQLCODE;-------------------------------
  PROCEDURE getobjecttypes(
    p_rc_objecttypes                   OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_objecttypes FOR
      SELECT   objtyp.object_type_id, objtyp.object_type_desc, objtyp.created_by, objtyp.created_date,
               objtyp.updated_by, objtyp.updated_date
          FROM sec_object_type_new objtyp
      ORDER BY objtyp.object_type_id;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getobjecttypes;

--------------------------------------------------------------------------------
--
-- Purpose:   Save users profile
--
-- Usage:     saveusers
--
--------------------------------------------------------------------------------
--Parse Users list and get the table object
    --loop the table object to insert/update/delete
  PROCEDURE saveusers(
    p_users                            IN     VARCHAR2,
    p_result                           OUT    VARCHAR2) IS
    v_list                                      VARCHAR2(32000);
    v_userrec                                   t_userrecord;
    v_assigndgroups                             VARCHAR2(32000);
  BEGIN
    v_list := p_users;

    --Loop users list
    WHILE LENGTH(p_users) <> 0 LOOP
      --get user profile values
      v_userrec.user_id := TO_NUMBER(SUBSTR(v_list, 1, INSTR(v_list, '~') - 1));
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_userrec.user_code := SUBSTR(v_list, 1, INSTR(v_list, '~') - 1);
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_userrec.first_name := SUBSTR(v_list, 1, INSTR(v_list, '~') - 1);
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_userrec.last_name := SUBSTR(v_list, 1, INSTR(v_list, '~') - 1);
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_userrec.effective_date := TO_DATE(SUBSTR(v_list, 1, INSTR(v_list, '~') - 1));
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_userrec.end_date := TO_DATE(SUBSTR(v_list, 1, INSTR(v_list, '~') - 1));
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_userrec.is_active := TO_NUMBER(SUBSTR(v_list, 1, INSTR(v_list, '~') - 1));
      v_list := SUBSTR(v_list, INSTR(v_list, '~') + 1);
      v_assigndgroups := SUBSTR(v_list, 1, INSTR(v_list, '|') - 1);
      v_list := SUBSTR(v_list, INSTR(v_list, '|') + 1);
    /*
    --Call the stored proc to insert/update/delete the user
    saveuser (v_userrec.user_id,
              v_userrec.user_code,
              v_userrec.first_name,
              v_userrec.last_name,
              v_userrec.effective_date,
              v_userrec.end_date,
              v_userrec.is_active,
              v_assigndgroups,
              p_result
             );
    */
    END LOOP;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_result := 'Error Occurred while updating User Profile - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END saveusers;

--------------------------------------------------------------------------------
--
-- Purpose:   Save user profile
--
-- Usage:     saveuser
--
--------------------------------------------------------------------------------
  PROCEDURE saveuser(
    p_user_id                          IN OUT NUMBER,
    p_user_code                        IN     VARCHAR2,
    p_first_name                       IN     VARCHAR2,
    p_last_name                        IN     VARCHAR2,
    p_effective_date                   IN     DATE,
    p_end_date                         IN     DATE,
    p_is_active                        IN     NUMBER,
    p_dept_code_lk                     IN     NUMBER,
    p_user_title_lk                    IN     NUMBER,
    p_office_code_lk                   IN     NUMBER,
    p_business_area_lk                 IN     NUMBER,  
    p_group_assigned                   IN     VARCHAR2,
    p_change_user                      IN     VARCHAR2,
    p_error_message                    OUT    VARCHAR2) IS
    v_user_id                                   NUMBER;
    v_errholder                                 VARCHAR2(255);

    CURSOR cur_user IS
      SELECT        *
               FROM sec_user_new
              WHERE user_id = p_user_id
      FOR UPDATE OF user_code, first_name, last_name, effective_date, end_date, is_active NOWAIT;

    v_user                                      cur_user%ROWTYPE;
    e_error_during_save                         EXCEPTION;
    e_user_already_exists                       EXCEPTION;
    v_count                                     NUMBER;
  BEGIN
    IF p_user_id < 0 THEN   -- New User
      SELECT COUNT(1)
        INTO v_count
        FROM sec_user_new
       WHERE user_code = p_user_code;

      IF v_count > 0 THEN
        RAISE e_user_already_exists;
      END IF;

      INSERT INTO sec_user_new
                  (user_id, user_code, first_name, last_name, effective_date, end_date,
                   is_active, created_by, created_date)
           VALUES (seq_user_id.NEXTVAL, p_user_code, p_first_name, p_last_name, p_effective_date, p_end_date,
                   p_is_active, p_change_user, SYSDATE)
        RETURNING user_id
             INTO v_user_id;

      --Inserting User atributes
      INSERT INTO gen_user_attributes
                  (user_id, dept_code_lk, user_title_lk, office_code_lk, business_area_lk, created_by, created_date)
           VALUES (v_user_id, p_dept_code_lk, p_user_title_lk, p_office_code_lk,p_business_area_lk, p_change_user, SYSDATE);

      --Create user group assignments
      savegroupassignment(v_user_id, p_group_assigned, 'A', p_change_user, p_error_message);

      IF p_error_message IS NOT NULL THEN
        p_error_message := 'User Group Assignments' || p_error_message;
        RAISE e_error_during_save;
      END IF;

      p_user_id := v_user_id;
    ELSE   -- Update an existing User
      OPEN cur_user;

      FETCH cur_user
       INTO v_user;

      IF cur_user%NOTFOUND THEN
        CLOSE cur_user;

        RAISE NO_DATA_FOUND;
      END IF;

      UPDATE sec_user_new
         SET user_code = p_user_code,
             first_name = p_first_name,
             last_name = p_last_name,
             effective_date = p_effective_date,
             end_date = p_end_date,
             is_active = p_is_active,
             updated_by = p_change_user,
             updated_date = SYSDATE
       WHERE CURRENT OF cur_user;

      CLOSE cur_user;

      --Update User atributes
      UPDATE gen_user_attributes
         SET dept_code_lk = p_dept_code_lk,
             user_title_lk = p_user_title_lk,
             office_code_lk = p_office_code_lk,
             business_area_lk = p_business_area_lk,
             updated_by = p_change_user,
             updated_date = SYSDATE
       WHERE user_id = p_user_id;

      IF SQL%ROWCOUNT = 0 THEN
        INSERT INTO gen_user_attributes
                    (user_id, dept_code_lk, user_title_lk, office_code_lk, business_area_lk, created_by, created_date)
             VALUES (p_user_id, p_dept_code_lk, p_user_title_lk, p_office_code_lk, p_business_area_lk, p_change_user, SYSDATE);
      END IF;

      --Update user group assignments
      savegroupassignment(p_user_id, p_group_assigned, 'U', p_change_user, p_error_message);

      IF p_error_message IS NOT NULL THEN
        p_error_message := 'User Group Assignments' || p_error_message;
        RAISE e_error_during_save;
      END IF;

      p_error_message := '';
    END IF;
  EXCEPTION
    WHEN e_user_already_exists THEN
      p_error_message := 'User already exists';
    WHEN NO_DATA_FOUND THEN
      p_error_message := 'Error Occurred while updating User Profile - ' || SQLERRM;
    WHEN OTHERS THEN
      p_error_message := 'Unknown Error in SaveUser: ' || SQLERRM;
  END saveuser;

--------------------------------------------------------------------------------
--
-- Purpose:   Save the user group assignments
--
-- Usage:     savegroupassignment
--
--------------------------------------------------------------------------------
  PROCEDURE savegroupassignment(
    p_user_id                          IN     NUMBER,
    p_group_assigned                   IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_change_user                      IN     VARCHAR2,
    p_result                           OUT    VARCHAR2) IS
    CURSOR cur_groups(
      c_user_id                          IN     NUMBER,
      c_group_id                         IN     NUMBER) IS
      SELECT COUNT(*)
        FROM sec_group_assignment_new ga
       WHERE ga.user_id = c_user_id AND ga.user_group_id = c_group_id;

    CURSOR cur_delgroups(
      c_user_id                          IN     NUMBER) IS
      SELECT ga.user_id, ga.user_group_id
        FROM sec_group_assignment_new ga
       WHERE ga.user_id = c_user_id;

    v_dataset                                   VARCHAR2(32000);
    v_groups                                    appsecurity_new.t_idrec;
    v_listlen                                   NUMBER(10);
    v_count                                     NUMBER(10);
    v_exists                                    BOOLEAN := FALSE;
    e_no_rows_updated                           EXCEPTION;
  BEGIN
    v_dataset := p_group_assigned;
    blobtobinary(v_dataset, p_result, v_listlen, v_groups);

    IF p_result IS NULL THEN
      FOR i IN 1 .. v_groups.COUNT LOOP
        IF p_save_type = 'A' THEN
          --create User Group Assignment
          INSERT INTO sec_group_assignment_new
                      (user_id, user_group_id, created_by, created_date)
               VALUES (p_user_id, v_groups(i), p_change_user, SYSDATE);
        ELSIF p_save_type = 'U' THEN
          OPEN cur_groups(p_user_id, v_groups(i));

          FETCH cur_groups
           INTO v_count;

          IF v_count = 0 THEN
            --create User Group Assignment
            INSERT INTO sec_group_assignment_new
                        (user_id, user_group_id, created_by, created_date)
                 VALUES (p_user_id, v_groups(i), p_change_user, SYSDATE);
          END IF;

          CLOSE cur_groups;
        END IF;   -- save type..
      END LOOP;

      --Check for the groups to be removed
      IF p_save_type = 'U' THEN
        FOR v_rec IN cur_delgroups(p_user_id) LOOP
          v_exists := FALSE;

          FOR i IN 1 .. v_groups.COUNT LOOP
            IF v_rec.user_group_id = v_groups(i) THEN
              v_exists := TRUE;
              EXIT WHEN v_exists;
            END IF;
          END LOOP;

          IF v_exists = FALSE THEN
            --Delete the record..
            DELETE FROM sec_group_assignment_new
                  WHERE user_id = p_user_id AND user_group_id = v_rec.user_group_id;
          END IF;
        END LOOP;   -- delete cursor loop..
      END IF;   -- save type..

      p_result := '';
    END IF;   -- p_result = null check..
  EXCEPTION
    WHEN e_no_rows_updated THEN
      p_result := 'No rows found to update user group assignment - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END savegroupassignment;

--------------------------------------------------------------------------------
--
-- Purpose:   Save the group details
--
-- Usage:     savegroup
--
--------------------------------------------------------------------------------
  PROCEDURE SaveGroup(
    p_group_id                         IN OUT NUMBER,
    p_group_name                       IN     VARCHAR2,
    p_group_desc                       IN     VARCHAR2,
    p_members                          IN     SCRIBE_PKG.t_IdRec,
    p_object_assigned                  IN     SCRIBE_PKG.t_IdRec,
    p_object_rights                    IN     SCRIBE_PKG.t_IdRec,
    p_rc_group                         OUT    scribe_pkg.t_generic_cursor,
    p_rc_member_users                  OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor,
    p_change_user                      IN     VARCHAR2,
    p_error_message                    IN OUT VARCHAR2) IS
    v_group_id                                  NUMBER;
    v_errholder                                 VARCHAR2(255);
    e_error_during_save                         EXCEPTION;
  BEGIN
    IF p_group_id < 0 THEN
      INSERT INTO sec_group_new
                  (user_group_id, user_group_name, user_group_desc, created_by, created_date)
           VALUES (seq_user_group_id.NEXTVAL, p_group_name, p_group_desc, p_change_user, SYSDATE)
        RETURNING user_group_id
             INTO p_group_id;
    ELSE
      UPDATE sec_group_new
         SET user_group_name = p_group_name,
             user_group_desc = p_group_desc,
             updated_by = p_change_user,
             updated_date = SYSDATE
       WHERE user_group_id = p_group_id;
    END IF;

    --Create user group assignments
    DELETE FROM sec_group_assignment_new
          WHERE user_group_id = p_group_id;

    FOR v_index IN p_members.FIRST .. p_members.LAST LOOP
      IF p_members(v_index) > 0 THEN
        INSERT INTO sec_group_assignment_new
                    (user_id, user_group_id, created_by, created_date)
             VALUES (p_members(v_index), p_group_id, p_change_user, SYSDATE);
      END IF;
    END LOOP;

    --Create object group assignments
    DELETE FROM sec_group_object_priv_new
          WHERE user_group_id = p_group_id;

    FOR v_index IN p_object_assigned.FIRST .. p_object_assigned.LAST LOOP
      IF p_object_rights(v_index) != 0 THEN
        INSERT INTO sec_group_object_priv_new
                    (object_id, user_group_id, access_rights, created_by, created_date)
             VALUES (p_object_assigned(v_index), p_group_id, p_object_rights(v_index), p_change_user, SYSDATE);
      END IF;
    END LOOP;

    GetGroupDetails(p_group_id, p_rc_group, p_rc_member_users, p_rc_objects_assigned);
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_error_message := 'Error Occurred while updating Group Info - ' || SQLERRM;
    WHEN OTHERS THEN
      p_error_message := SQLERRM;
  END SaveGroup;

--------------------------------------------------------------------------------
--
-- Purpose:   Save the user group assignments
--
-- Usage:     savegroupmembers
--
--------------------------------------------------------------------------------
  PROCEDURE savegroupmembers(
    p_group_id                         IN     NUMBER,
    p_members                          IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_result                           OUT    VARCHAR2) IS
    CURSOR cur_users(
      c_group_id                         IN     NUMBER,
      c_user_id                          IN     NUMBER) IS
      SELECT COUNT(*)
        FROM sec_group_assignment_new ga
       WHERE ga.user_group_id = c_group_id AND ga.user_id = c_user_id;

    CURSOR cur_delusers(
      c_group_id                         IN     NUMBER) IS
      SELECT ga.user_id, ga.user_group_id
        FROM sec_group_assignment_new ga
       WHERE ga.user_group_id = c_group_id;

    i                                           NUMBER;
    v_dataset                                   VARCHAR2(32000);
    v_users                                     appsecurity_new.t_idrec;
    v_listlen                                   NUMBER(10);
    v_count                                     NUMBER(10);
    v_exists                                    BOOLEAN := FALSE;
    e_no_rows_updated                           EXCEPTION;
  BEGIN
    v_dataset := p_members;
    blobtobinary(v_dataset, p_result, v_listlen, v_users);

    IF p_result IS NULL THEN
      FOR i IN 1 .. v_users.COUNT LOOP
        IF p_save_type = 'A' THEN
          --create User Group Assignment
          INSERT INTO sec_group_assignment_new
                      (user_group_id, user_id)
               VALUES (p_group_id, v_users(i));
        ELSIF p_save_type = 'U' THEN
          OPEN cur_users(p_group_id, v_users(i));

          FETCH cur_users
           INTO v_count;

          IF v_count = 0 THEN
            --create User Group Assignment
            INSERT INTO sec_group_assignment_new
                        (user_group_id, user_id)
                 VALUES (p_group_id, v_users(i));
          END IF;

          CLOSE cur_users;
        END IF;   -- save type..
      END LOOP;

      --Check for the groups to be removed
      IF p_save_type = 'U' THEN
        FOR v_rec IN cur_delusers(p_group_id) LOOP
          v_exists := FALSE;

          FOR i IN 1 .. v_users.COUNT LOOP
            IF v_rec.user_id = v_users(i) THEN
              v_exists := TRUE;
              EXIT WHEN v_exists;
            END IF;
          END LOOP;

          IF v_exists = FALSE THEN
            --Delete the record..
            DELETE FROM sec_group_assignment_new
                  WHERE user_group_id = p_group_id AND user_id = v_rec.user_id;
          END IF;
        END LOOP;   -- delete cursor loop..
      END IF;   -- save type..

      p_result := '';
    END IF;   -- p_result = null check..
  EXCEPTION
    WHEN e_no_rows_updated THEN
      p_result := 'No rows found to update user group assignment - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END savegroupmembers;

--------------------------------------------------------------------------------
--
-- Purpose:   Save the group's assignment with objects
--
-- Usage:     saveobjectassignment
--
--------------------------------------------------------------------------------
  PROCEDURE savegroupobjectassignment(
    p_group_id                         IN     NUMBER,
    p_object_assigned                  IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_result                           OUT    VARCHAR2) IS
    CURSOR cur_groupobj(
      c_group_id                         IN     NUMBER,
      c_object_id                        IN     NUMBER) IS
      SELECT COUNT(*)
        FROM sec_group_object_priv_new gp
       WHERE gp.user_group_id = c_group_id AND gp.object_id = c_object_id;

    CURSOR cur_objects(
      c_group_id                         IN     NUMBER,
      c_object_id                        IN     NUMBER) IS
      SELECT        object_id, access_rights
               FROM sec_group_object_priv_new gp
              WHERE gp.user_group_id = c_group_id AND gp.object_id = c_object_id
      FOR UPDATE OF access_rights NOWAIT;

    CURSOR cur_delobjects(
      c_group_id                         IN     NUMBER) IS
      SELECT gp.user_group_id, gp.object_id
        FROM sec_group_object_priv_new gp
       WHERE gp.user_group_id = c_group_id;

    v_dataset                                   VARCHAR2(32000);
    v_listlen                                   NUMBER(10);
    v_count                                     NUMBER(10);
    v_rec                                       cur_delobjects%ROWTYPE;
    v_obj_priv_rec                              t_objprivilege;
    v_exists                                    BOOLEAN := FALSE;
    v_object_privs                              t_objprivileges;
    e_no_rows_updated                           EXCEPTION;
  BEGIN
    v_dataset := p_object_assigned;
    --Parse the object privileges blob..
    getobjectprivsrec(v_dataset, v_object_privs, p_result);

    IF p_result IS NULL THEN
      FOR i IN 1 .. v_object_privs.COUNT LOOP
        IF p_save_type = 'A' THEN
          --create Group Object Assignment
          INSERT INTO sec_group_object_priv_new
                      (user_group_id, object_id, access_rights)
               VALUES (p_group_id, v_object_privs(i).object_id, v_object_privs(i).access_rights);
        ELSIF p_save_type = 'U' THEN
          OPEN cur_groupobj(p_group_id, v_object_privs(i).object_id);

          FETCH cur_groupobj
           INTO v_count;

          IF v_count = 0 THEN
            --create Group Object Assignment
            INSERT INTO sec_group_object_priv_new
                        (user_group_id, object_id, access_rights)
                 VALUES (p_group_id, v_object_privs(i).object_id, v_object_privs(i).access_rights);
          ELSE
            OPEN cur_objects(p_group_id, v_object_privs(i).object_id);

            FETCH cur_objects
             INTO v_obj_priv_rec;

            UPDATE sec_group_object_priv_new
               SET access_rights = v_object_privs(i).access_rights
             WHERE CURRENT OF cur_objects;

            CLOSE cur_objects;
          END IF;

          CLOSE cur_groupobj;
        END IF;   -- save type..
      END LOOP;

      --Check for the groups to be removed
      IF p_save_type = 'U' THEN
        FOR v_rec IN cur_delobjects(p_group_id) LOOP
          v_exists := FALSE;

          FOR i IN 1 .. v_object_privs.COUNT LOOP
            IF v_rec.object_id = v_object_privs(i).object_id THEN
              v_exists := TRUE;
              EXIT WHEN v_exists;
            END IF;
          END LOOP;

          IF v_exists = FALSE THEN
            --Delete the record..
            DELETE FROM sec_group_object_priv_new
                  WHERE user_group_id = p_group_id AND object_id = v_rec.object_id;
          END IF;
        END LOOP;   -- delete cursor loop..
      END IF;   -- save type..

      p_result := '';
    END IF;   -- p_result = null check..
  EXCEPTION
    WHEN e_no_rows_updated THEN
      p_result := 'No rows found to update group objects assignment - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END savegroupobjectassignment;

--------------------------------------------------------------------------------
--
-- Purpose: Creates array of object and privileges assigned to them for a group
--
-- Usage:    GetObjectPrivsRec
--
--------------------------------------------------------------------------------
  PROCEDURE getobjectprivsrec(
    p_data_set                         IN     VARCHAR2,
    p_data_list                        OUT    t_objprivileges,
    p_result                           OUT    VARCHAR2) IS
    v_data_set                                  VARCHAR2(32000);
    i                                           NUMBER(10) := 1;
  BEGIN
    v_data_set := p_data_set;

    WHILE LENGTH(v_data_set) <> 0 LOOP
      p_data_list(i).object_id := TO_NUMBER(SUBSTR(v_data_set, 1, INSTR(v_data_set, '~') - 1));
      v_data_set := SUBSTR(v_data_set, INSTR(v_data_set, '~') + 1);
      p_data_list(i).access_rights := TO_NUMBER(SUBSTR(v_data_set, 1, INSTR(v_data_set, '|') - 1));
      v_data_set := SUBSTR(v_data_set, INSTR(v_data_set, '|') + 1);
      i := i + 1;
    END LOOP;

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := 'Error While parsing the Object Privileges blob - ' || SQLERRM;
  END getobjectprivsrec;

  -- Purpose(SaveObject):
  --         Save the objects detail info
  PROCEDURE saveobject(
    p_object_id                        IN OUT NUMBER,
    p_object_name                      IN     VARCHAR2,
    p_parent_id                        IN     NUMBER,
    p_prog_name                        IN     VARCHAR2,
    p_object_type_id                   IN     NUMBER,
    p_object_desc                      IN     VARCHAR2,
    p_object_text                      IN     VARCHAR2,
    p_object_icon_key                  IN     VARCHAR2,
    p_assembly_name                    IN     VARCHAR2,
    p_display_order                    IN     NUMBER,
    p_is_active                        IN     NUMBER,
    p_is_node                          IN     NUMBER,
    p_help_topic_id                    IN     NUMBER,
    p_group_id_assigned                IN     scribe_pkg.t_IdRec,
    p_group_rights                     IN     scribe_pkg.t_IdRec,
    p_rc_objects                       OUT    scribe_pkg.t_generic_cursor,
    p_rc_object_groups                 OUT    scribe_pkg.t_generic_cursor,
    p_change_user                      IN     VARCHAR2,
    p_result                           OUT    VARCHAR2) IS
    v_object_id                                 NUMBER;
    v_errholder                                 VARCHAR2(255);
    v_index                                     NUMBER;
  BEGIN
    IF p_object_id < 0 THEN
      INSERT INTO sec_object_new
                  (object_id, object_name, parent_object_id, object_prog_name, object_type_id,
                   object_desc, object_text, object_icon_key, assembly_name, display_order, is_active,
                   is_node, help_topic_id, created_by, created_date)
           VALUES (seq_object_id.NEXTVAL, p_object_name, p_parent_id, NVL(p_prog_name, ' '), p_object_type_id,
                   p_object_desc, p_object_text, p_object_icon_key, p_assembly_name, p_display_order, p_is_active,
                   p_is_node, p_help_topic_id, p_change_user, SYSDATE)
        RETURNING object_id
             INTO p_object_id;
    ELSE   -- Update an existing Group
      UPDATE sec_object_new
         SET object_name = p_object_name,
             parent_object_id = p_parent_id,
             object_prog_name = p_prog_name,
             object_type_id = p_object_type_id,
             object_desc = p_object_desc,
             object_text = p_object_text,
             object_icon_key = p_object_icon_key,
             assembly_name = p_assembly_name,
             display_order = p_display_order,
             is_active = p_is_active,
             is_node = p_is_node,
             help_topic_id = p_help_topic_id,
             updated_by = p_change_user,
             updated_date = SYSDATE
       WHERE object_id = p_object_id;

      p_result := '';
    END IF;

    FOR v_index IN p_group_id_assigned.FIRST .. p_group_id_assigned.LAST LOOP
      UPDATE sec_group_object_priv_new
         SET access_rights = p_group_rights(v_index),
             updated_by = p_change_user,
             updated_date = SYSDATE
       WHERE object_id = p_object_id AND user_group_id = p_group_id_assigned(v_index);

      IF SQL%ROWCOUNT = 0 THEN
        INSERT INTO sec_group_object_priv_new
                    (object_id, user_group_id, access_rights, created_by, created_date)
             VALUES (p_object_id, p_group_id_assigned(v_index), p_group_rights(v_index), p_change_user, SYSDATE);
      END IF;
    END LOOP;

    GetObject(p_object_id, p_rc_objects, p_rc_object_groups);
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_result := 'Error Occurred while updating Object Info - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END saveobject;

--------------------------------------------------------------------------------
--
-- Purpose:   Save the object's assignment with groups
--
-- Usage:     saveobjectgroupassignment
--
--------------------------------------------------------------------------------
  PROCEDURE saveobjectgroupassignment(
    p_object_id                        IN     NUMBER,
    p_group_assigned                   IN     VARCHAR2,
    p_save_type                        IN     VARCHAR2,
    p_result                           OUT    VARCHAR2) IS
    CURSOR cur_groups(
      c_object_id                        IN     NUMBER,
      c_group_id                         IN     NUMBER) IS
      SELECT COUNT(*)
        FROM sec_group_object_priv_new gp
       WHERE gp.object_id = c_object_id AND gp.user_group_id = c_group_id;

    CURSOR cur_delgroups(
      c_object_id                        IN     NUMBER) IS
      SELECT gp.object_id, gp.user_group_id
        FROM sec_group_object_priv_new gp
       WHERE gp.object_id = c_object_id;

    v_dataset                                   VARCHAR2(32000);
    v_groups                                    appsecurity_new.t_idrec;
    v_listlen                                   NUMBER(10);
    v_count                                     NUMBER(10);
    v_exists                                    BOOLEAN := FALSE;
    e_no_rows_updated                           EXCEPTION;
  BEGIN
    v_dataset := p_group_assigned;
    blobtobinary(v_dataset, p_result, v_listlen, v_groups);

    IF p_result IS NULL THEN
      FOR i IN 1 .. v_groups.COUNT LOOP
        IF p_save_type = 'A' THEN
          --create Group Object Assignment
          INSERT INTO sec_group_object_priv_new
                      (object_id, user_group_id)   --,
               --access_rights)
          VALUES      (p_object_id, v_groups(i));
        ELSIF p_save_type = 'U' THEN
          OPEN cur_groups(p_object_id, v_groups(i));

          FETCH cur_groups
           INTO v_count;

          IF v_count = 0 THEN
            --create Group Object Assignment
            INSERT INTO sec_group_object_priv_new
                        (object_id, user_group_id)   --,
                 --access_rights)
            VALUES      (p_object_id, v_groups(i));
          END IF;

          CLOSE cur_groups;
        END IF;   -- save type..
      END LOOP;

      --Check for the groups to be removed
      IF p_save_type = 'U' THEN
        FOR v_rec IN cur_delgroups(p_object_id) LOOP
          v_exists := FALSE;

          FOR i IN 1 .. v_groups.COUNT LOOP
            IF v_rec.user_group_id = v_groups(i) THEN
              v_exists := TRUE;
              EXIT WHEN v_exists;
            END IF;
          END LOOP;

          IF v_exists = FALSE THEN
            --Delete the record..
            DELETE FROM sec_group_object_priv_new
                  WHERE object_id = p_object_id AND user_group_id = v_rec.user_group_id;
          END IF;
        END LOOP;   -- delete cursor loop..
      END IF;   -- save type..

      p_result := '';
    END IF;   -- p_result = null check..
  EXCEPTION
    WHEN e_no_rows_updated THEN
      p_result := 'No rows found to update group objects assignment - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END saveobjectgroupassignment;

  FUNCTION IsUserInGroup(
    p_user_code                        IN     VARCHAR2,
    p_group_name                       IN     VARCHAR2)
    RETURN NUMBER IS
    v_count                                     NUMBER;
  BEGIN
    SELECT COUNT(1)
      INTO v_count
      FROM sec_user_new usr, sec_group_assignment_new grpa, sec_group_new grp
     WHERE usr.user_id = grpa.user_id
       AND grpa.user_group_id = grp.user_group_id
       AND usr.user_code = p_user_code
       AND grp.user_group_name = p_group_name;

    RETURN v_count;
  END IsUserInGroup;

--------------------------------------------------------------------------------
--
-- Purpose:   get the groups and objects assigned to a user
--
-- Usage:     getgroupsassigned
--
--------------------------------------------------------------------------------
  PROCEDURE getgroupsassigned(
    p_user_code                        IN     VARCHAR,
    p_rc_groups                        OUT    scribe_pkg.t_generic_cursor,
    p_rc_objects_assigned              OUT    scribe_pkg.t_generic_cursor) IS
    v_errholder                                 VARCHAR2(255);
  BEGIN
    OPEN p_rc_groups FOR
      SELECT   sg.user_group_id, sg.user_group_name, sg.user_group_desc
          FROM sec_group_new sg, sec_user_new su, sec_group_assignment_new ga
         WHERE su.user_code = p_user_code AND su.user_id = ga.user_id AND ga.user_group_id = sg.user_group_id
      ORDER BY sg.user_group_name;

    --Get the objects assigned to each group
    OPEN p_rc_objects_assigned FOR
      SELECT   gp.user_group_id, oh.object_id, oh.object_name, oh.parent_object_id, oh.object_prog_name,
               oh.object_type_id, oh.object_type_desc, oh.object_desc, oh.object_text, oh.object_icon_key,
               oh.assembly_name, oh.display_order, oh.is_active, oh.is_node, gp.access_rights, oh.object_level
          FROM sec_user_new su,
               sec_group_assignment_new ga,
               sec_group_object_priv_new gp,
               (SELECT     o.object_id, o.object_name, o.parent_object_id, o.object_prog_name, o.object_type_id,
                           ot.object_type_desc, o.object_desc, o.object_text, o.object_icon_key, o.assembly_name,
                           o.display_order, o.is_active, o.is_node, LEVEL object_level
                      FROM sec_object_new o, sec_object_type_new ot
                     WHERE o.is_active = 1 AND o.object_type_id = ot.object_type_id
                START WITH o.object_id = 1
                CONNECT BY PRIOR o.object_id = o.parent_object_id
                  ORDER SIBLINGS BY display_order) oh
         WHERE su.user_code = p_user_code
           AND su.is_active = 1
           --AND su.end_date IS null
           AND su.user_id = ga.user_id
           AND gp.user_group_id = ga.user_group_id
           AND gp.object_id = oh.object_id
      ORDER BY gp.user_group_id, oh.object_level;
  EXCEPTION
    WHEN OTHERS THEN
      v_errholder := SQLERRM;
  END getgroupsassigned;

--------------------------------------------------------------------------------
--
-- Purpose:   delete user
--
-- Usage:     deleteuser
--
--------------------------------------------------------------------------------
  PROCEDURE deleteuser(
    p_user_id                          IN     NUMBER,
    p_result                           OUT    VARCHAR2) IS
  BEGIN
    --delete user to group assignment
    DELETE      sec_group_assignment_new
          WHERE user_id = p_user_id;

    --delete user
    DELETE      sec_user_new
          WHERE user_id = p_user_id;

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END deleteuser;

--------------------------------------------------------------------------------
--
-- Purpose:   delete group
--
-- Usage:     deletegroup
--
--------------------------------------------------------------------------------
  PROCEDURE deletegroup(
    p_group_id                         IN     NUMBER,
    p_result                           OUT    VARCHAR2) IS
  BEGIN
    --delete group to user assignment
    DELETE      sec_group_assignment_new
          WHERE user_group_id = p_group_id;

    --delete group to object assignment
    DELETE      sec_group_object_priv_new
          WHERE user_group_id = p_group_id;

    --delete group
    DELETE      sec_group_new
          WHERE user_group_id = p_group_id;

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END deletegroup;

--------------------------------------------------------------------------------
--
-- Purpose:   delete object
--
-- Usage:     deleteobject
--
--------------------------------------------------------------------------------
  PROCEDURE deleteobject(
    p_object_id                        IN     NUMBER,
    p_result                           OUT    VARCHAR2) IS
  BEGIN
    --delete object's and child object's assignment to the group
    DELETE      sec_group_object_priv_new
          WHERE object_id IN(SELECT     o.object_id
                                   FROM sec_object_new o, sec_object_type_new ot
                                  WHERE o.object_type_id = ot.object_type_id
                             START WITH object_id = p_object_id
                             CONNECT BY PRIOR object_id = parent_object_id);

    --delete object and children
    DELETE      sec_object_new
          WHERE object_id IN(SELECT     o.object_id
                                   FROM sec_object_new o, sec_object_type_new ot
                                  WHERE o.object_type_id = ot.object_type_id
                             START WITH object_id = p_object_id
                             CONNECT BY PRIOR object_id = parent_object_id);

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END deleteobject;

  -- Purpose(GetObjectsAccessRightsForUser):
  --         Returns the Access rights for objects the specified or all users have.
  PROCEDURE GetObjectsAccessRightsForUser(
    p_user_code                        IN     VARCHAR2,
    p_rc                               OUT    SCRIBE_PKG.t_generic_cursor,
    p_error_message                    IN OUT VARCHAR2) IS
    v_user_id                                   NUMBER;
    UNKNOWN_USER_EXCEP                          EXCEPTION;
  BEGIN
    v_user_id := UserIdFromCode(p_user_code);

    IF v_user_id = -1 THEN
      RAISE UNKNOWN_USER_EXCEP;
    END IF;

    OPEN p_rc FOR
      SELECT     user_id, object_id, object_name, object_desc, object_prog_name, assembly_name, parent_object_id, object_type_desc,
                 object_text, object_icon_key, is_active, is_node, help_topic_id, display_order,
                 DECODE(hasdelete, 0, 0, 1) has_delete, DECODE(hasupdate, 0, 0, 1) has_update,
                 DECODE(hasread, 0, 0, 1) has_read, DECODE(hascreate, 0, 0, 1) has_create, LEVEL
            FROM (SELECT   grpa.user_id, obj.object_id, obj.object_name, obj.object_desc, obj.object_prog_name, obj.assembly_name,
                           obj.parent_object_id, objt.object_type_desc, obj.object_text, obj.object_icon_key,
                           obj.is_active, obj.is_node, NVL(obj.help_topic_id, 0) help_topic_id, obj.display_order,
                           SUM(BITAND(gop.access_rights, 8)) hasdelete, SUM(BITAND(gop.access_rights, 4)) hasupdate,
                           SUM(BITAND(gop.access_rights, 2)) hasread, SUM(BITAND(gop.access_rights, 1)) hascreate
                      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop, sec_object_new obj,
                           sec_object_type_new objt, sec_user_new usr
                     WHERE grpa.user_group_id(+) = gop.user_group_id
                       AND gop.object_id(+) = obj.object_id
                       AND obj.object_type_id(+) = objt.object_type_id
                       AND grpa.user_id = usr.user_id
                       AND TRUNC(SYSDATE) BETWEEN TRUNC(usr.effective_date) AND TRUNC(NVL(usr.end_date, SYSDATE + 1))
                       AND usr.user_id = v_user_id
                  GROUP BY grpa.user_id, obj.object_id, obj.object_name, obj.object_desc, obj.object_prog_name, obj.assembly_name,
                           obj.parent_object_id, objt.object_type_desc, obj.object_text, obj.object_icon_key,
                           obj.is_active, obj.is_node, obj.help_topic_id, obj.display_order)
      START WITH parent_object_id IS NULL
      CONNECT BY parent_object_id = PRIOR object_id AND user_id = PRIOR user_id
        ORDER SIBLINGS BY user_id, display_order;
  EXCEPTION
    WHEN UNKNOWN_USER_EXCEP THEN
      p_error_message := 'Provided user does not exist';
    WHEN OTHERS THEN
      IF SQLERRM IS NOT NULL THEN
        p_error_message := p_error_message || ' ' || SQLERRM;
      END IF;
  END GetObjectsAccessRightsForUser;   -- End of Function GetObjectsAccessRightsByUsers

  -- Purpose(GetBBObjectGroup):
  --         Returns the group assigned to the passed object. Used for Blackberry Reports
  PROCEDURE GetBBObjectGroup(
    p_object_name                      IN     VARCHAR2,
    p_rc                               OUT    SCRIBE_PKG.t_generic_cursor,
    p_error_message                    IN OUT VARCHAR2) IS
  BEGIN
    OPEN p_rc FOR
      SELECT grp.user_group_name
        FROM sec_object_new obj, sec_group_object_priv_new gop, sec_group_new grp
       WHERE obj.object_id = gop.object_id
         AND gop.user_group_id = grp.user_group_id
         AND obj.object_name = p_object_name
         AND gop.access_rights = 15;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_error_message := SQLERRM;
  END GetBBObjectGroup;   -- End of Procedure GetBBObjectGroup

  FUNCTION UserHasCreateForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER IS
    v_hascreate                                 NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 1)), 0)
      INTO v_hascreate
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_hascreate != 0 THEN
      v_hascreate := 1;
    END IF;

    RETURN v_hascreate;
  END UserHasCreateForObject;   -- End of Function UserHasCreateForObject

  FUNCTION UserHasReadForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 2)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END UserHasReadForObject;   -- End of Function UserHasReadForObject

  FUNCTION UserHasUpdateForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 4)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END UserHasUpdateForObject;   -- End of Function UserHasUpdateForObject

  FUNCTION UserHasUpdateOnObject(
    p_user_code                        IN     VARCHAR2,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
    v_user_id                                   NUMBER;
      
  BEGIN
    v_user_id := UserIdFromCode(p_user_code);
  
    SELECT NVL(SUM(BITAND(gop.access_rights, 4)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = v_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
    
  END UserHasUpdateOnObject;   -- End of Function UserHasUpdateOnObject

  FUNCTION UserHasDeleteForObject(
    p_user_id                          IN     NUMBER,
    p_object_id                        IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 8)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END UserHasDeleteForObject;   -- End of Function UserHasDeleteForObject

  FUNCTION ObjectHasCreateForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 1)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ObjectHasCreateForGroup;   -- End of Function ObjectHasCreateForGroup

  FUNCTION ObjectHasReadForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 2)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ObjectHasReadForGroup;   -- End of Function ObjectHasReadForGroup

  FUNCTION ObjectHasUpdateForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 4)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ObjectHasUpdateForGroup;   -- End of Function ObjectHasUpdateForGroup

  FUNCTION ObjectHasDeleteForGroup(
    p_object_id                        IN     NUMBER,
    p_group_id                         IN     NUMBER)
    RETURN NUMBER IS
    v_has                                       NUMBER;
  BEGIN
    SELECT NVL(SUM(BITAND(gop.access_rights, 8)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ObjectHasDeleteForGroup;   -- End of Function ObjectHasDeleteForGroup

  --Purpose(GetObjectParentHierarchyName):
  --        Returns the parent hierarchy information
  FUNCTION GetObjectParentHierarchyName(
    p_object_id                        IN     NUMBER)
    RETURN VARCHAR2 IS
    CURSOR cr_hierarchy(
      c_object_id                        IN     NUMBER) IS
      SELECT     obj.object_id, NVL(obj.parent_object_id, 0) parent_object_id, obj.object_text
            FROM sec_object_new obj
      CONNECT BY PRIOR obj.parent_object_id = obj.object_id
      START WITH obj.object_id = c_object_id;

    v_hierarchy                                 VARCHAR2(1000) := '';
  BEGIN
    FOR v_obj IN cr_hierarchy(p_object_id) LOOP
      IF v_obj.object_id <> p_object_id AND v_obj.parent_object_id <> 0 THEN
        IF LENGTH(v_hierarchy) > 0 THEN
          v_hierarchy := ' > ' || v_hierarchy;
        END IF;

        v_hierarchy := v_obj.object_text || v_hierarchy;
      END IF;
    END LOOP;

    RETURN v_hierarchy;
  END GetObjectParentHierarchyName;

  FUNCTION GetObjectSortLevel(
    p_object_id                        IN     NUMBER)
    RETURN NUMBER IS
    v_sort_level                                NUMBER;
  BEGIN
    SELECT SUM(sort_order)
      INTO v_sort_level
      FROM (SELECT     POWER(10, objtyp.level_number) * obj.display_order sort_order
                  FROM sec_object_new obj, sec_object_type_new objtyp
                 WHERE obj.object_type_id = objtyp.object_type_id
            CONNECT BY PRIOR obj.parent_object_id = obj.object_id
            START WITH obj.object_id = p_object_id);

    RETURN v_sort_level;
  END GetObjectSortLevel;

--------------------------------------------------------------------------------
-- Private Procedure : BlobToBinary
--
-- Purpose:
--
--------------------------------------------------------------------------------
  PROCEDURE BlobToBinary(
    p_blob                             IN     VARCHAR2,
    p_result                           OUT    VARCHAR2,
    p_ListLen                          OUT    NUMBER,
    p_BinaryList                       OUT    t_BinaryRec) IS
    v_blob                                      VARCHAR2(32000);
    i                                           NUMBER(10) := 1;
  BEGIN
    v_blob := p_blob;

    WHILE LENGTH(v_blob) <> 0 LOOP
      IF INSTR(v_blob, '~') = 0 THEN
        p_BinaryList(i) := v_blob;
        v_blob := '';
      ELSE
        p_BinaryList(i) := TO_NUMBER(SUBSTR(v_blob, 1, INSTR(v_blob, '~') - 1));
        v_blob := SUBSTR(v_blob, INSTR(v_blob, '~') + 1);
        i := i + 1;
      END IF;
    END LOOP;

    --IF i = 1 THEN
    --   p_result := 'Blob contains no Tildas';
    --ELSE
    --   p_result := '';
    --END IF;
    p_result := '';
    p_ListLen := i - 1;
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END BlobToBinary;

--------------------------------------------------------------------------------
-- Private Procedure : BlobToBinary
--
-- Purpose:
--
--------------------------------------------------------------------------------
  PROCEDURE BlobToBinary(
    p_blob                             IN     VARCHAR2,
    p_result                           OUT    VARCHAR2,
    p_ListLen                          OUT    NUMBER,
    p_BinaryList                       OUT    t_IdRec) IS
    v_blob                                      VARCHAR2(32000);
    i                                           NUMBER(10) := 1;
  BEGIN
    v_blob := p_blob;

    WHILE LENGTH(v_blob) <> 0 LOOP
      IF INSTR(v_blob, '~') = 0 THEN
        p_BinaryList(i) := v_blob;
        v_blob := '';
      ELSE
        p_BinaryList(i) := TO_NUMBER(SUBSTR(v_blob, 1, INSTR(v_blob, '~') - 1));
        v_blob := SUBSTR(v_blob, INSTR(v_blob, '~') + 1);
        i := i + 1;
      END IF;
    END LOOP;

    /*IF i = 1 THEN
       p_result := 'Blob contains no Tildas';
    ELSE
       p_result := '';
    END IF; */
    p_result := '';
    p_ListLen := i - 1;
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END BlobToBinary;

--------------------------------------------------------------------------------
  FUNCTION UserIdFromCode(
    p_user_code                        IN     VARCHAR2)
    RETURN NUMBER IS
    v_user_id                                   NUMBER;
  BEGIN
    SELECT user_id
      INTO v_user_id
      FROM sec_user_new
     WHERE user_code = p_user_code;

    RETURN v_user_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      RETURN -1;
  END UserIdFromCode;
END;   -- End of DDL Script for Package Body APPSECURITY_NEW
/
