CREATE SCHEMA appsecurity_new

DECLARE t_generic_cursor REFCURSOR;

DECLARE TYPE t_BinaryRec IS TABLE OF NUMBER(1) INDEX BY BINARY_INTEGER;

  TYPE t_IdRec IS TABLE OF NUMBER(10) INDEX BY BINARY_INTEGER;

  -- TYPE t_objprivilege IS RECORD(
    -- object_id                                   NUMBER(10),
    -- access_rights                               NUMBER(3)
  -- );

  TYPE t_objprivileges IS TABLE OF t_objprivilege INDEX BY BINARY_INTEGER;

  -- TYPE t_userrecord IS RECORD(
    -- user_id                                     sec_user_new.user_id%TYPE,
    -- user_code                                   sec_user_new.user_code%TYPE,
    -- first_name                                  sec_user_new.first_name%TYPE,
    -- last_name                                   sec_user_new.last_name%TYPE,
    -- effective_date                              sec_user_new.effective_date%TYPE,
    -- end_date                                    sec_user_new.end_date%TYPE,
    -- is_active                                   sec_user_new.is_active%TYPE
  -- );   --t_userrecord

  TYPE t_users IS TABLE OF t_userrecord INDEX BY BINARY_INTEGER;

  -- TYPE t_grouprecord IS RECORD(
    -- GROUP_ID                                    sec_group_new.user_group_id%TYPE,
    -- group_name                                  sec_group_new.user_group_name%TYPE,
    -- group_desc                                  sec_group_new.user_group_desc%TYPE
  -- );   --t_grouprecord

  TYPE t_groups IS TABLE OF t_grouprecord INDEX BY BINARY_INTEGER;

  -- TYPE t_objectrecord IS RECORD(
    -- object_id                                   sec_object_new.object_id%TYPE,
    -- object_name                                 sec_object_new.object_name%TYPE,
    -- parent_id                                   sec_object_new.parent_object_id%TYPE,
    -- prog_name                                   sec_object_new.object_prog_name%TYPE,
    -- object_type_id                              sec_object_new.object_type_id%TYPE,
    -- object_desc                                 sec_object_new.object_desc%TYPE,
    -- object_text                                 sec_object_new.object_text%TYPE,
    -- object_icon_key                             sec_object_new.object_icon_key%TYPE,
    -- assembly_name                               sec_object_new.assembly_name%TYPE,
    -- display_order                               sec_object_new.display_order%TYPE,
    -- active_ir                                   sec_object_new.is_active%TYPE
  -- );   --t_objectrecord

  TYPE t_objects IS TABLE OF t_objectrecord INDEX BY BINARY_INTEGER;
  
    CREATE OR REPLACE FUNCTION getusers(p_rc_users OUT refcursor, p_rc_groups_assigned  OUT  refcursor) AS $$
    DECLARE
	  v_errholder VARCHAR(255);
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
    END;
    $$ LANGUAGE plpgsql;
  
  
  
  
    CREATE OR REPLACE FUNCTION getgroups() RETURNS refcursor AS $$
    DECLARE
      p_rc_groups refcursor;
	  v_errholder VARCHAR(255);
    BEGIN
      OPEN p_rc_groups FOR SELECT   user_group_id, user_group_name, user_group_desc
          FROM sec_group_new
      ORDER BY user_group_name;
	EXCEPTION
    WHEN OTHERS THEN
		v_errholder := SQLERRM;
		
      RETURN p_rc_groups;
    END;
    $$ LANGUAGE plpgsql;
  
  
  
  CREATE OR REPLACE FUNCTION getuserdetails(p_user_id NUMERIC,
	OUT p_rc_user_details refcursor,
	OUT p_rc_groups_assigned refcursor,
	OUT p_rc_objects_assigned refcursor)
    RETURNS record AS $$
    DECLARE
     v_errholder VARCHAR(255);	  
    BEGIN
      OPEN p_rc_user_details FOR
      SELECT   usr.user_id, usr.user_code, usr.first_name, usr.last_name, usr.effective_date, usr.end_date,
               usr.is_active, dept_code_lk, user_title_lk, business_area_lk, office_code_lk, usr.created_by, usr.created_date,
               usr.updated_by, usr.updated_date
          FROM sec_user_new usr, gen_user_attributes gu
         WHERE usr.user_id = gu.user_id AND usr.user_id = p_user_id
      ORDER BY usr.user_code;
                                         
 --Get the groups assigned to the user
    OPEN p_rc_groups_assigned FOR
      SELECT   ga.user_id, ga.user_group_id, g.user_group_name, g.user_group_desc
          FROM sec_group_new g, sec_group_assignment_new ga
         WHERE g.user_group_id = ga.user_group_id AND ga.user_id = p_user_id
      ORDER BY g.user_group_name;										 
 
    --Get the objects assigned to each group the user belongs
    OPEN p_rc_objects_assigned FOR
      WITH RECURSIVE tmp AS (SELECT obj.object_id, obj.object_name, obj.parent_object_id, obj.object_prog_name, obj.object_desc,
                 obj.object_text, obj.object_icon_key, obj.assembly_name, obj.display_order, obj.is_active,
                 obj.is_node, COALESCE(obj.help_topic_id, 0) help_topic_id, objt.object_type_desc,
                 UserHasCreateForObject(p_user_id, obj.object_id) Has_Create,
                 UserHasReadForObject(p_user_id, obj.object_id) Has_Read,
                 UserHasUpdateForObject(p_user_id, obj.object_id) Has_Update,
                 UserHasDeleteForObject(p_user_id, obj.object_id) Has_Delete
            FROM sec_object_new obj, sec_object_type_new objt
           WHERE obj.object_type_id = objt.object_type_id AND obj.object_id = 1
           UNION
              SELECT obj.object_id, obj.object_name, obj.parent_object_id, obj.object_prog_name, obj.object_desc,
                 obj.object_text, obj.object_icon_key, obj.assembly_name, obj.display_order, obj.is_active,
                 obj.is_node, COALESCE(obj.help_topic_id, 0) help_topic_id, objt.object_type_desc,
                 UserHasCreateForObject(p_user_id, obj.object_id) Has_Create,
                 UserHasReadForObject(p_user_id, obj.object_id) Has_Read,
                 UserHasUpdateForObject(p_user_id, obj.object_id) Has_Update,
                 UserHasDeleteForObject(p_user_id, obj.object_id) Has_Delete, LEVEL
            FROM sec_object_new obj, sec_object_type_new objt
          JOIN tmp ON tmp.object_id = obj.parent_object_id)
		SELECT object_id, object_name, parent_object_id, object_prog_name, object_desc,
                 object_text, object_icon_key, assembly_name, display_order, is_active,
                 is_node, COALESCE(help_topic_id, 0) help_topic_id, object_type_desc,
                 UserHasCreateForObject(p_user_id, object_id) Has_Create,
                 UserHasReadForObject(p_user_id, object_id) Has_Read,
                 UserHasUpdateForObject(p_user_id, object_id) Has_Update,
                 UserHasDeleteForObject(p_user_id, object_id) Has_Delete, LEVEL FROM tmp
        ORDER BY tmp.display_order;
		
  EXCEPTION
    WHEN OTHERS THEN
	      v_errholder := SQLERRM;
	END ;
    $$ LANGUAGE plpgsql;
  
  
  
 CREATE OR REPLACE FUNCTION getusers(OUT p_rc_users refcursor) RETURNS refcursor AS $$
    DECLARE                             
	  v_errholder VARCHAR(255);	  
    BEGIN
      OPEN p_rc_users FOR
    SELECT
        su.user_id, su.user_code, su.first_name, su.last_name, su.effective_date, su.end_date, su.is_active, dept_code_lk, user_title_lk, office_code_lk, (SELECT
            lookup_name
            FROM gen_lookup_item
            WHERE lookup_id = dept_code_lk) AS dept_name, business_area_lk, (SELECT
            lookup_name
            FROM gen_lookup_item
            WHERE lookup_id = business_area_lk) AS business_area_name, (SELECT
            lookup_name
            FROM gen_lookup_item
            WHERE lookup_id = user_title_lk) AS user_title, (SELECT
            lookup_name
            FROM gen_lookup_item
            WHERE lookup_id = office_code_lk) AS office_name, su.created_by, su.created_date, su.updated_by, su.updated_date
        FROM sec_user_new AS su
        LEFT OUTER JOIN gen_user_attributes AS gu
            ON (su.user_id = gu.user_id)
        ORDER BY su.user_code;
    EXCEPTION
        WHEN others THEN
            v_errholder := SQLERRM;
	END ;
    $$ LANGUAGE plpgsql;
  
  
  
CREATE OR REPLACE FUNCTION getgroupdetails(p_user_group_id NUMERIC,
	OUT p_rc_group refcursor,
	OUT p_rc_member_users refcursor,
	OUT p_rc_objects_assigned refcursor)
    RETURNS record AS $$
    DECLARE
       v_errholder VARCHAR(255);	  
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
      WITH RECURSIVE tmp AS (SELECT     obj.object_id, LPAD(' ', LEVEL * 3) || obj.object_name object_name, obj.parent_object_id,
                 obj.display_order, obj.has_delete, obj.has_update, obj.has_read, obj.has_create
            FROM (SELECT   obj.object_id, obj.object_name, obj.parent_object_id, obj.display_order,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 8)) = 0 THEN has_delete = 0 ELSE has_delete = 1 END,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 4)) = 0 THEN has_update = 0 ELSE has_update = 1 END,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 2)) = 0 THEN has_read = 0 ELSE has_read = 1 END,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 1)) = 0 THEN has_create = 0 ELSE has_create = 1 END
                           --DECODE(SUM((COALESCE(gop.access_rights, 0) & 8)), 0, 0, 1) has_delete,
                          -- DECODE(SUM((COALESCE(gop.access_rights, 0) & 4)), 0, 0, 1) has_update,
                           --DECODE(SUM((COALESCE(gop.access_rights, 0) & 2)), 0, 0, 1) has_read,
                           --DECODE(SUM((COALESCE(gop.access_rights, 0) & 1)), 0, 0, 1) has_create
                      FROM sec_object_new obj, sec_group_object_priv_new gop
                     WHERE gop.object_id = obj.object_id AND gop.user_group_id = p_user_group_id
                  GROUP BY obj.object_id, obj.object_name, obj.parent_object_id, obj.display_order) obj
           WHERE obj.object_id = 1 UNION SELECT obj.object_id, LPAD(' ', LEVEL * 3) || obj.object_name object_name, obj.parent_object_id,
                 obj.display_order, obj.has_delete, obj.has_update, obj.has_read, obj.has_create
            FROM (SELECT   obj.object_id, obj.object_name, obj.parent_object_id, obj.display_order,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 8)) = 0 THEN has_delete = 0 ELSE has_delete = 1 END,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 4)) = 0 THEN has_update = 0 ELSE has_update = 1 END,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 2)) = 0 THEN has_read = 0 ELSE has_read = 1 END,
			CASE WHEN SUM((COALESCE(gop.access_rights, 0) & 1)) = 0 THEN has_create = 0 ELSE has_create = 1 END
                           --DECODE(SUM((COALESCE(gop.access_rights, 0) & 8)), 0, 0, 1) has_delete,
                           ----DECODE(SUM((COALESCE(gop.access_rights, 0) & 4)), 0, 0, 1) has_update,
                           --DECODE(SUM((COALESCE(gop.access_rights, 0) & 2)), 0, 0, 1) has_read,
                           --DECODE(SUM((COALESCE(gop.access_rights, 0) & 1)), 0, 0, 1) has_create
                      FROM sec_object_new obj, sec_group_object_priv_new gop
                    WHERE gop.object_id = obj.object_id AND gop.user_group_id = p_user_group_id
                  GROUP BY obj.object_id, obj.object_name, obj.parent_object_id, obj.display_order) obj
           JOIN tmp ON tmp.object_id = obj.parent_object_id )
SELECT object_id, LPAD(' ', LEVEL * 3) || object_name object_name, parent_object_id, display_order, has_delete, has_update, has_read, has_create FROM tmp ORDER BY tmp.display_order;
	  
  EXCEPTION
    WHEN OTHERS THEN
	     v_errholder := SQLERRM;
	END;
    $$ LANGUAGE plpgsql;
  
  
  
   CREATE OR REPLACE FUNCTION GetObjects() RETURNS refcursor AS $$
    DECLARE
      p_rc_objects refcursor;   
    BEGIN
      OPEN p_rc_objects FOR
      WITH RECURSIVE tmp AS (SELECT     o.object_id, LPAD(' ', LEVEL * 3) || o.object_name object_name, o.parent_object_id,
                 ot.object_type_desc, o.object_text, o.object_icon_key
            FROM sec_object_new o, sec_object_type_new ot
           WHERE o.object_type_id = ot.object_type_id AND o.object_id = 1 UNION SELECT o.object_id, LPAD(' ', LEVEL * 3) || o.object_name object_name, o.parent_object_id,
                 ot.object_type_desc, o.object_text, o.object_icon_key
            FROM sec_object_new o JOIN sec_object_type_new ot ON o.object_type_id = ot.object_type_id JOIN tmp ON tmp.object_id = o.parent_object_id )
SELECT object_id, LPAD(' ', LEVEL * 3) || object_name object_name, parent_object_id, object_type_desc, object_text, object_icon_key FROM tmp ORDER BY tmp.display_order;
    RETURN p_rc_group;
	
	END ;
    $$ LANGUAGE plpgsql;
  
  
  
 CREATE OR REPLACE FUNCTION GetObject(p_object_id NUMERIC,
	OUT p_rc_objects refcursor,
	OUT p_rc_object_groups refcursor)
    RETURNS record AS $$
    DECLARE
       v_errholder CHARACTER VARYING(255);
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
             ObjectHasCreateForGroup(p_object_id, grp.user_group_id) has_create,
             ObjectHasUpdateForGroup(p_object_id, grp.user_group_id) has_update,
             ObjectHasReadForGroup(p_object_id, grp.user_group_id) has_read,
             ObjectHasDeleteForGroup(p_object_id, grp.user_group_id) has_delete
        FROM sec_group_new grp;
	
  EXCEPTION
    WHEN OTHERS THEN
	      v_errholder := SQLERRM;
	
	END ;
    $$ LANGUAGE plpgsql;
  
  
  
  CREATE OR REPLACE FUNCTION getobjecttypes() RETURNS refcursor AS $$
    DECLARE  
	  p_rc_objecttypes refcursor;
	  v_errholder VARCHAR(255);
    BEGIN
      OPEN p_rc_objecttypes FOR
      SELECT   objtyp.object_type_id, objtyp.object_type_desc, objtyp.created_by, objtyp.created_date,
               objtyp.updated_by, objtyp.updated_date
          FROM sec_object_type_new objtyp
      ORDER BY objtyp.object_type_id;
    
	
  EXCEPTION
    WHEN OTHERS THEN
	      v_errholder := SQLERRM;
	RETURN p_rc_objecttypes;
	END ;
    $$ LANGUAGE plpgsql;

  
  
  
  
 CREATE OR REPLACE FUNCTION saveusers(p_users VARCHAR,OUT p_result VARCHAR) RETURNS VARCHAR AS $$
    DECLARE  
	  v_list   VARCHAR(32000);
	  v_userrec  record;
      v_assigndgroups  VARCHAR(32000);
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
    WHEN no_data_found THEN
            p_result := CONCAT_WS('', 'Error Occurred while updating User Profile - ', SQLERRM);
    WHEN OTHERS THEN
      p_result := SQLERRM;

	END ;
    $$ LANGUAGE plpgsql;
  
  
  
  
  CREATE OR REPLACE FUNCTION saveuser(
 INOUT p_user_id  NUMERIC,
    p_user_code  VARCHAR,
    p_first_name  VARCHAR,
    p_last_name  VARCHAR,
    p_effective_date  DATE,
    p_end_date  DATE,
    p_is_active NUMERIC,
    p_dept_code_lk  NUMERIC,
    p_user_title_lk  NUMERIC,
    p_office_code_lk   NUMERIC,
    p_business_area_lk  NUMERIC,  
    p_group_assigned   VARCHAR,
    p_change_user  VARCHAR, OUT   p_error_message VARCHAR)
	RETURNS record
  AS $$
  DECLARE 
	v_user_id NUMERIC;
    v_errholder VARCHAR(255);
    cur_user CURSOR FOR
    SELECT
        *
        FROM sec_user_new
        WHERE user_id = p_user_id
        FOR UPDATE OF SEC_USER_NEW, SEC_USER_NEW, SEC_USER_NEW, SEC_USER_NEW, SEC_USER_NEW, SEC_USER_NEW NOWAIT;
    v_user record;
    v_count NUMERIC;
BEGIN
    IF p_user_id < 0
    /* New User */
    THEN
        SELECT
            COUNT(1)
            INTO STRICT v_count
            FROM sec_user_new
            WHERE user_code = p_user_code;

        IF v_count > 0 THEN
            RAISE EXCEPTION 'e_user_already_exists' USING errcode='50001';
        END IF;
        INSERT INTO sec_user_new (user_id, user_code, first_name, last_name, effective_date, end_date, is_active, created_by, created_date)
        VALUES (nextval('seq_user_id'), p_user_code, p_first_name, p_last_name, p_effective_date, p_end_date, p_is_active, p_change_user, CURRENT_DATE)
        RETURNING user_id INTO v_user_id
        /* Inserting User atributes */;
        INSERT INTO gen_user_attributes (user_id, dept_code_lk, user_title_lk, office_code_lk, business_area_lk, created_by, created_date)
        VALUES (v_user_id, p_dept_code_lk, p_user_title_lk, p_office_code_lk, p_business_area_lk, p_change_user, CURRENT_DATE)
        /* Create user group assignments */;
        SELECT
            *
            FROM savegroupassignment(v_user_id, p_group_assigned, 'A'::TEXT, p_change_user)
            INTO p_error_message;

        IF p_error_message IS NOT NULL THEN
            p_error_message := CONCAT_WS('', 'User Group Assignments', p_error_message);
            --RAISE USING detail = e_error_during_save, hint = 1;
            RAISE EXCEPTION 'e_error_during_save' USING errcode='50001';
        END IF;
        p_user_id := v_user_id;
    /* Update an existing User */
    ELSE
        OPEN cur_user;
        FETCH cur_user INTO v_user;
        cur_user := FOUND;

        IF (NOT cur_user) THEN
            CLOSE cur_user;
            RAISE no_data_found USING hint = 'NO_DATA_FOUND';
        END IF;
        UPDATE sec_user_new
        SET user_code = p_user_code, first_name = p_first_name, last_name = p_last_name, effective_date = p_effective_date, end_date = p_end_date, is_active = p_is_active, updated_by = p_change_user, updated_date = CURRENT_DATE
            WHERE CURRENT OF cur_user;
        CLOSE cur_user
        /* Update User atributes */;
        UPDATE gen_user_attributes
        SET dept_code_lk = p_dept_code_lk, user_title_lk = p_user_title_lk, office_code_lk = p_office_code_lk, business_area_lk = p_business_area_lk, updated_by = p_change_user, updated_date = CURRENT_DATE
            WHERE user_id = p_user_id;
        --GET DIAGNOSTICS sql$rowcount = ROW_COUNT;

        IF sql$rowcount = 0 THEN
            INSERT INTO gen_user_attributes (user_id, dept_code_lk, user_title_lk, office_code_lk, business_area_lk, created_by, created_date)
            VALUES (p_user_id, p_dept_code_lk, p_user_title_lk, p_office_code_lk, p_business_area_lk, p_change_user, CURRENT_DATE);
        END IF
        /* Update user group assignments */;
        SELECT
            *
            FROM savegroupassignment(p_user_id, p_group_assigned, 'U'::TEXT, p_change_user)
            INTO p_error_message;

        IF p_error_message IS NOT NULL THEN
            p_error_message := CONCAT_WS('', 'User Group Assignments', p_error_message);
            RAISE EXCEPTION 'e_error_during_save' USING errcode='50001';
        END IF;
        p_error_message := '';
    END IF;
    EXCEPTION
        WHEN SQLSTATE '50001' then
            p_error_message := 'User already exists';
        WHEN no_data_found THEN
            p_error_message := CONCAT_WS('', 'Error Occurred while updating User Profile - ', SQLERRM);
        WHEN others THEN
            p_error_message := CONCAT_WS('', 'Unknown Error in SaveUser: ', SQLERRM);
	END;
  $$
  LANGUAGE plpgsql ;
  
  
  
  
 
 CREATE OR REPLACE FUNCTION savegroupassignment(p_user_id NUMERIC,p_group_assigned VARCHAR, p_save_type VARCHAR, p_change_user VARCHAR,OUT p_result VARCHAR) RETURNS VARCHAR AS $$
    DECLARE  
	  cur_groups  CURSOR(c_user_id NUMERIC, c_group_id NUMERIC) FOR SELECT COUNT(*)
        FROM sec_group_assignment_new ga
       WHERE ga.user_id = c_user_id AND ga.user_group_id = c_group_id;
	   
	   cur_delgroups CURSOR (c_user_id      NUMERIC) FOR SELECT ga.user_id, ga.user_group_id
        FROM sec_group_assignment_new ga
       WHERE ga.user_id = c_user_id;

    v_dataset                                   VARCHAR(32000);
    v_groups                                    VARCHAR(100) := 'v_groups';
    v_listlen                                   NUMERIC(10);
    v_count                                     NUMERIC(10);
    v_exists                                    BOOLEAN := FALSE;
    --e_no_rows_updated EXCEPTION;
    F$RESULT_REC RECORD;
    BEGIN
      
    v_dataset := p_group_assigned;
    SELECT
        *
        FROM blobtobinary(v_dataset, v_groups, '')
        INTO F$RESULT_REC;
    p_result := F$RESULT_REC.P_RESULT;
    v_listlen := F$RESULT_REC.P_LISTLEN;

    IF p_result IS NULL THEN
        FOR i IN 1..v_groups LOOP
            IF p_save_type = 'A'
            /* create User Group Assignment */
            THEN
                INSERT INTO sec_group_assignment_new (user_id, user_group_id, created_by, created_date)
                VALUES (p_user_id, v_groups(i), p_change_user, CURRENT_DATE);
            ELSIF p_save_type = 'U' THEN
                OPEN cur_groups (p_user_id, v_groups(i));
                FETCH cur_groups INTO v_count;

                IF v_count = 0
                /* create User Group Assignment */
                THEN
                    INSERT INTO sec_group_assignment_new (user_id, user_group_id, created_by, created_date)
                    VALUES (p_user_id, v_groups(i), p_change_user, CURRENT_DATE);
                END IF;
                CLOSE cur_groups;
            END IF;
            /* save type.. */
        END LOOP
        /* Check for the groups to be removed */;

        IF p_save_type = 'U' THEN
            FOR v_rec IN cur_delgroups (p_user_id) LOOP
                v_exists := FALSE;

                FOR i IN 1..v_groups LOOP
                    IF v_rec.user_group_id = v_groups(i) THEN
                        v_exists := TRUE;
                        EXIT WHEN v_exists;
                    END IF;
                END LOOP;

                IF v_exists = FALSE
                /* Delete the record.. */
                THEN
                    DELETE FROM sec_group_assignment_new
                        WHERE user_id = p_user_id AND user_group_id = v_rec.user_group_id;
                END IF;
            END LOOP;
        /* delete cursor loop.. */
        END IF
        /* save type.. */;
        p_result := '';
    END IF
    /* p_result = null check.. */;
    
    EXCEPTION
        --WHEN e_no_rows_updated THEN
             --p_result := CONCAT_WS('', 'No rows found to update user group assignment - ', SQLERRM);
        WHEN others THEN
            p_result := SQLERRM;
	END ;
    $$ LANGUAGE plpgsql;
  
  
  
  
  CREATE OR REPLACE FUNCTION SaveGroup(INOUT p_group_id NUMERIC,  p_group_name  VARCHAR, p_group_desc  VARCHAR, p_members  VARCHAR,
	p_object_assigned VARCHAR,	
	p_object_rights VARCHAR,	
	OUT p_rc_group refcursor,
	OUT p_rc_member_users refcursor,
	OUT p_rc_objects_assigned refcursor,
	p_change_user text,
	INOUT p_error_message text)
    RETURNS record
  AS $$
  DECLARE v_group_id                                  NUMERIC;
    v_errholder                                 VARCHAR(255);
	BEGIN
    
    IF p_group_id < 0 THEN
        INSERT INTO sec_group_new (user_group_id, user_group_name, user_group_desc, created_by, created_date)
        VALUES (nextval('seq_user_group_id'), p_group_name, p_group_desc, p_change_user, CURRENT_DATE)
        RETURNING user_group_id INTO p_group_id;
    ELSE
        UPDATE sec_group_new
        SET user_group_name = p_group_name, user_group_desc = p_group_desc, updated_by = p_change_user, updated_date = CURRENT_DATE
            WHERE user_group_id = p_group_id;
    END IF
    /* Create user group assignments */;
    DELETE FROM sec_group_assignment_new
        WHERE user_group_id = p_group_id;

    FOR v_index IN p_object_assigned.FIRST .. p_object_assigned.LAST LOOP
        IF p_members(v_index) > 0 THEN
            INSERT INTO sec_group_assignment_new (user_id, user_group_id, created_by, created_date)
            VALUES (p_members(v_index), p_group_id, p_change_user, CURRENT_DATE);
        END IF;
    END LOOP
    /* Create object group assignments */;
    DELETE FROM sec_group_object_priv_new
        WHERE user_group_id = p_group_id;

    FOR v_index IN p_object_assigned.FIRST .. p_object_assigned.LAST LOOP
        IF p_object_rights(v_index) != 0 THEN
            INSERT INTO sec_group_object_priv_new (object_id, user_group_id, access_rights, created_by, created_date)
            VALUES (p_object_assigned(v_index), p_group_id, p_object_rights(v_index), p_change_user, CURRENT_DATE);
        END IF;
    END LOOP;
    SELECT
        *
        FROM getgroupdetails(p_group_id)
        INTO p_rc_group, p_rc_member_users, p_rc_objects_assigned;
    
    EXCEPTION
        WHEN no_data_found THEN
            p_error_message := CONCAT_WS('', 'Error Occurred while updating Group Info - ', SQLERRM);
        WHEN others THEN
            p_error_message := SQLERRM;
	END;
  $$
  LANGUAGE plpgsql ;
  
  
  
  
CREATE OR REPLACE FUNCTION savegroupmembers(p_group_id NUMERIC,p_members VARCHAR, p_save_type VARCHAR,OUT p_result VARCHAR) RETURNS VARCHAR AS $$
    DECLARE  
	  cur_users CURSOR (c_group_id      NUMERIC,c_user_id     NUMERIC) FOR SELECT COUNT(*)
        FROM sec_group_assignment_new ga
       WHERE ga.user_group_id = c_group_id AND ga.user_id = c_user_id;
	   
	   cur_delusers CURSOR (c_group_id  NUMERIC) FOR SELECT ga.user_id, ga.user_group_id
        FROM sec_group_assignment_new ga
       WHERE ga.user_group_id = c_group_id;

    i                                           NUMERIC;
    v_dataset                                   VARCHAR(32000);
    v_users                                     VARCHAR(100) := 'v_users';
    v_listlen                                   NUMERIC(10);
    v_count                                     NUMERIC(10);
    v_exists                                    BOOLEAN := FALSE;
    F$RESULT_REC RECORD;
    BEGIN
      v_dataset := p_members;
      SELECT
        *
        FROM blobtobinary(v_dataset, v_users, '')
        INTO F$RESULT_REC;
    p_result := F$RESULT_REC.P_RESULT;
    v_listlen := F$RESULT_REC.P_LISTLEN;

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
        WHEN others THEN
            p_result := SQLERRM;
	
	END ;
    $$ LANGUAGE plpgsql;
  
  
  
  
  

  CREATE OR REPLACE FUNCTION savegroupobjectassignment(p_group_id NUMERIC,p_object_assigned VARCHAR, p_save_type VARCHAR,OUT p_result VARCHAR) RETURNS VARCHAR AS $$
    DECLARE  
	  cur_groupobj CURSOR( c_group_id       NUMERIC, c_object_id    NUMERIC) FOR  SELECT COUNT(*)
        FROM sec_group_object_priv_new gp
		WHERE gp.user_group_id = c_group_id AND gp.object_id = c_object_id;

    cur_objects CURSOR(c_group_id    NUMERIC,c_object_id      NUMERIC) FOR  SELECT   object_id, access_rights
               FROM sec_group_object_priv_new gp
              WHERE gp.user_group_id = c_group_id AND gp.object_id = c_object_id
      FOR UPDATE OF access_rights NOWAIT;

    cur_delobjects CURSOR (c_group_id      NUMERIC) FOR  SELECT gp.user_group_id, gp.object_id
        FROM sec_group_object_priv_new gp
       WHERE gp.user_group_id = c_group_id;

    v_dataset                                   VARCHAR(32000);
    v_listlen                                   NUMERIC(10);
    v_count                                     NUMERIC(10);
    v_rec                                       record;
    v_obj_priv_rec                              record;
    v_exists                                    BOOLEAN := FALSE;
    v_object_privs                              record;
    
    F$RESULT_REC RECORD;
    BEGIN
      v_dataset := p_object_assigned;
    --Parse the object privileges blob..
   SELECT
        *
        FROM getobjectprivsrec(v_dataset, v_object_privs, 'getobjectprivsrec')
        INTO F$RESULT_REC;
    p_result := F$RESULT_REC.P_RESULT;

    IF p_result IS NULL THEN
      FOR i IN 1 .. v_object_privs.COUNT LOOP
        IF p_save_type = 'A' THEN
          --create Group Object Assignment
          INSERT INTO sec_group_object_priv_new (user_group_id, object_id, access_rights)
                VALUES (p_group_id, v_object_privs(i), v_object_privs(i));
        ELSIF p_save_type = 'U' THEN
          OPEN cur_groupobj (p_group_id, v_object_privs(i));
                FETCH cur_groupobj INTO v_count;

          IF v_count = 0 THEN
            --create Group Object Assignment
           INSERT INTO sec_group_object_priv_new (user_group_id, object_id, access_rights)
                VALUES (p_group_id, v_object_privs(i), v_object_privs(i));
          ELSE
            OPEN cur_objects (p_group_id, v_object_privs(i));
                    FETCH cur_objects INTO v_obj_priv_rec;
                    UPDATE sec_group_object_priv_new
                    SET access_rights = v_object_privs(i)
                        WHERE CURRENT OF cur_objects;
                    CLOSE cur_objects;
                END IF;
                CLOSE cur_groupobj;
            END IF;
      END LOOP;

      --Check for the groups to be removed
      IF p_save_type = 'U' THEN
        FOR v_rec IN cur_delobjects(p_group_id) LOOP
          v_exists := FALSE;

          FOR i IN 1..v_object_privs.COUNT LOOP
                    IF v_rec.object_id = v_object_privs(i) THEN
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
        WHEN others THEN
            p_result := SQLERRM;
	END ;	
    $$ LANGUAGE plpgsql;
  
  
  
  
  CREATE OR REPLACE FUNCTION getobjectprivsrec( p_data_set  VARCHAR,
 OUT   p_data_list character varying,"p_data_list$function_name" character varying,
 OUT p_result  VARCHAR)
  AS $$
  DECLARE v_data_set     VARCHAR(32000);
    i    NUMERIC(10) := 1;
	BEGIN
    v_data_set := p_data_set;

    WHILE LENGTH(v_data_set) <> 0 LOOP
        PERFORM aws_oracle_ext.array$set_value('p_data_list[' || i || '].object_id', 'getobjectprivsrec', aws_oracle_ext.TO_NUMBER(aws_oracle_ext.substr(v_data_set, 1, aws_oracle_ext.INSTR(v_data_set, '~') - 1)));
        v_data_set := aws_oracle_ext.substr(v_data_set, aws_oracle_ext.INSTR(v_data_set, '~') + 1);
        PERFORM aws_oracle_ext.array$set_value('p_data_list[' || i || '].access_rights', 'getobjectprivsrec', aws_oracle_ext.TO_NUMBER(aws_oracle_ext.substr(v_data_set, 1, aws_oracle_ext.INSTR(v_data_set, '|') - 1)));
        v_data_set := aws_oracle_ext.substr(v_data_set, aws_oracle_ext.INSTR(v_data_set, '|') + 1);
        i := i + 1;
    END LOOP;
    p_result := '';
    PERFORM aws_oracle_ext.array$assign('p_data_list', 'getobjectprivsrec', p_data_list, p_data_list$function_name);
    PERFORM aws_oracle_ext.array$clear_procedure('getobjectprivsrec');
    EXCEPTION
        WHEN others THEN
            p_result := CONCAT_WS('', 'Error While parsing the Object Privileges blob - ', SQLERRM);
	END ;
  $$
  LANGUAGE plpgsql ;
  
  
  
  
 
 CREATE OR REPLACE FUNCTION saveobject(INOUT p_object_id NUMERIC,
   p_object_name    VARCHAR,
    p_parent_id     NUMERIC,
    p_prog_name   VARCHAR,
    p_object_type_id  NUMERIC,
    p_object_desc    VARCHAR,
    p_object_text     VARCHAR,
    p_object_icon_key    VARCHAR,
    p_assembly_name    VARCHAR,
    p_display_order    NUMERIC,
    p_is_active     NUMERIC,
    p_is_node      NUMERIC,
    p_help_topic_id    NUMERIC,
    p_group_id_assigned character varying,
	p_group_rights character varying,
  OUT  p_rc_objects     refcursor,
  OUT  p_rc_object_groups  refcursor,
    p_change_user    VARCHAR,
  OUT  p_result    VARCHAR)
  AS $$
  DECLARE v_object_id      NUMERIC;
    v_errholder    VARCHAR(255);
    v_index      NUMERIC;
	BEGIN
    IF p_object_id < 0 THEN
      INSERT INTO sec_object_new
                  (object_id, object_name, parent_object_id, object_prog_name, object_type_id,
                   object_desc, object_text, object_icon_key, assembly_name, display_order, is_active,
                   is_node, help_topic_id, created_by, created_date)
           VALUES (seq_object_id.NEXTVAL, p_object_name, p_parent_id, COALESCE(p_prog_name, ' '), p_object_type_id,
                   p_object_desc, p_object_text, p_object_icon_key, p_assembly_name, p_display_order, p_is_active,
                   p_is_node, p_help_topic_id, p_change_user, CURRENT_DATE)
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
             updated_date = CURRENT_DATE
       WHERE object_id = p_object_id;

      p_result := '';
    END IF;

    FOR v_index IN p_group_id_assigned.FIRST .. p_group_id_assigned.LAST LOOP
      UPDATE sec_group_object_priv_new
         SET access_rights = p_group_rights(v_index),
             updated_by = p_change_user,
             updated_date = CURRENT_DATE
       WHERE object_id = p_object_id AND user_group_id = p_group_id_assigned(v_index);

      IF SQL%ROWCOUNT = 0 THEN
        INSERT INTO sec_group_object_priv_new
                    (object_id, user_group_id, access_rights, created_by, created_date)
             VALUES (p_object_id, p_group_id_assigned(v_index), p_group_rights(v_index), p_change_user, CURRENT_DATE);
      END IF;
    END LOOP;

     SELECT
        *
        FROM getobject(p_object_id)
        INTO p_rc_objects, p_rc_object_groups;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_result := 'Error Occurred while updating Object Info - ' || SQLERRM;
    WHEN OTHERS THEN
      p_result := SQLERRM;
	END ;
  $$
  LANGUAGE plpgsql ;
  
  
  
  
 CREATE OR REPLACE FUNCTION saveobjectgroupassignment(p_object_id NUMERIC,p_group_assigned VARCHAR, p_save_type VARCHAR,OUT p_result VARCHAR) RETURNS VARCHAR AS $$
    DECLARE  
	  cur_groups CURSOR (c_object_id       NUMERIC,c_group_id    NUMERIC) FOR SELECT COUNT(*)
        FROM sec_group_object_priv_new gp
        WHERE gp.object_id = c_object_id AND gp.user_group_id = c_group_id;

  cur_delgroups CURSOR (c_object_id    NUMERIC) FOR   SELECT gp.object_id, gp.user_group_id
        FROM sec_group_object_priv_new gp
       WHERE gp.object_id = c_object_id;

    v_dataset                                   VARCHAR(32000);
    v_groups                                    VARCHAR(100) := 'v_groups';
    v_listlen                                   NUMERIC(10);
    v_count                                     NUMERIC(10);
    v_exists                                    BOOLEAN := FALSE;
    F$RESULT_REC RECORD;
    BEGIN
      v_dataset := p_group_assigned;
    SELECT
        *
        FROM blobtobinary(v_dataset, v_groups, '')
        INTO F$RESULT_REC;
    p_result := F$RESULT_REC.P_RESULT;
    v_listlen := F$RESULT_REC.P_LISTLEN;


    IF p_result IS NULL THEN
        FOR i IN 1..v_groups LOOP
            IF p_save_type = 'A'
            /* create Group Object Assignment */
            THEN
                INSERT INTO sec_group_object_priv_new (object_id, user_group_id)
                /* , */
                /* access_rights) */
                VALUES (p_object_id, v_groups(i));
            ELSIF p_save_type = 'U' THEN
                OPEN cur_groups (p_object_id, v_groups(i));
                FETCH cur_groups INTO v_count;

                IF v_count = 0
                /* create Group Object Assignment */
                THEN
                    INSERT INTO sec_group_object_priv_new (object_id, user_group_id)
                    /* , */
                    /* access_rights) */
                    VALUES (p_object_id, v_groups(i));
                END IF;
                CLOSE cur_groups;
            END IF;
            /* save type.. */
        END LOOP
        /* Check for the groups to be removed */;

        IF p_save_type = 'U' THEN
            FOR v_rec IN cur_delgroups (p_object_id) LOOP
                v_exists := FALSE;

                FOR i IN 1..v_groups.COUNT LOOP
                    IF v_rec.user_group_id = v_groups(i) THEN
                        v_exists := TRUE;
                        EXIT WHEN v_exists;
                    END IF;
                END LOOP;

                IF v_exists = FALSE
                /* Delete the record.. */
                THEN
                    DELETE FROM sec_group_object_priv_new
                        WHERE object_id = p_object_id AND user_group_id = v_rec.user_group_id;
                END IF;
            END LOOP;
        /* delete cursor loop.. */
        END IF
        /* save type.. */;
        p_result := '';
    END IF
    /* p_result = null check.. */;
    
    EXCEPTION
        WHEN others THEN
            p_result := SQLERRM;
	END ;	
    $$ LANGUAGE plpgsql;
  
  
  
  
CREATE OR REPLACE FUNCTION getgroupsassigned(p_user_code VARCHAR,
	OUT p_rc_groups refcursor,
	OUT p_rc_objects_assigned refcursor)
    RETURNS record AS $$
    DECLARE 
		v_errholder VARCHAR(255);	
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
               (WITH RECURSIVE tmp AS (SELECT  o.object_id, o.object_name, o.parent_object_id, o.object_prog_name, o.object_type_id,
                           ot.object_type_desc, o.object_desc, o.object_text, o.object_icon_key, o.assembly_name,
                           o.display_order, o.is_active, o.is_node, LEVEL object_level
                      FROM sec_object_new o, sec_object_type_new ot 
WHERE o.is_active = 1 AND o.object_type_id = ot.object_type_id AND o.object_id = 1
UNION 
SELECT o.object_id, o.object_name, o.parent_object_id, o.object_prog_name, o.object_type_id,
                           ot.object_type_desc, o.object_desc, o.object_text, o.object_icon_key, o.assembly_name,
                           o.display_order, o.is_active, o.is_node, LEVEL object_level
                      FROM sec_object_new o JOIN sec_object_type_new ot ON o.is_active = 1 AND o.object_type_id = ot.object_type_id
JOIN
tmp ON tmp.object_id = o.parent_object_id)
SELECT object_id, object_name, parent_object_id, object_prog_name, object_type_id, object_type_desc, object_desc, object_text, object_icon_key, assembly_name, display_order, is_active, is_node, LEVEL object_level FROM tmp ORDER BY tmp.display_order) oh
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

	END ;	
    $$ LANGUAGE plpgsql;
  
  
  
  
  
CREATE OR REPLACE FUNCTION deleteuser(p_user_id  NUMERIC) RETURNS VARCHAR AS $$
    DECLARE 
		p_result VARCHAR;	
    BEGIN
      --delete user to group assignment
    DELETE  FROM  sec_group_assignment_new
          WHERE user_id = p_user_id;

    --delete user
    DELETE FROM sec_user_new
          WHERE user_id = p_user_id;

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  RETURN p_result;
  END;	
    $$ LANGUAGE plpgsql;
  
  
  
  
CREATE OR REPLACE FUNCTION deletegroup(p_group_id  NUMERIC) RETURNS VARCHAR AS $$
    DECLARE 
		p_result VARCHAR;	
    BEGIN
    --delete group to user assignment
    DELETE FROM sec_group_assignment_new
          WHERE user_group_id = p_group_id;

    --delete group to object assignment
    DELETE FROM sec_group_object_priv_new
          WHERE user_group_id = p_group_id;

    --delete group
    DELETE FROM sec_group_new
          WHERE user_group_id = p_group_id;

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  RETURN p_result;
  END;	
    $$ LANGUAGE plpgsql;
  
  
  
 CREATE OR REPLACE FUNCTION deleteobject(p_object_id  NUMERIC) RETURNS VARCHAR AS $$
    DECLARE 
		p_result VARCHAR;	
    BEGIN
    --delete object's and child object's assignment to the group
    DELETE FROM sec_group_object_priv_new
          WHERE object_id IN(WITH RECURSIVE tmp AS (SELECT  o.object_id 
FROM sec_object_new o, sec_object_type_new ot 
WHERE o.object_type_id = ot.object_type_id AND object_id = p_object_id 
UNION 
SELECT o.object_id 
FROM sec_object_new o JOIN sec_object_type_new ot ON o.object_type_id = ot.object_type_id JOIN tmp ON
 tmp.object_id=parent_object_id)
SELECT object_id FROM tmp);

    --delete object and children
    DELETE FROM sec_object_new
          WHERE object_id IN(WITH RECURSIVE tmp AS (SELECT  o.object_id FROM sec_object_new o, sec_object_type_new ot 
WHERE o.object_type_id = ot.object_type_id AND object_id = p_object_id UNION SELECT o.object_id 
FROM sec_object_new o JOIN sec_object_type_new ot ON o.object_type_id = ot.object_type_id JOIN tmp ON
 tmp.object_id=parent_object_id)SELECT object_id FROM tmp);

    p_result := '';
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  RETURN p_result;
  END;	
    $$ LANGUAGE plpgsql;
  
  
  
  
  CREATE OR REPLACE FUNCTION GetObjectsAccessRightsForUser(
    p_user_code     VARCHAR,
  OUT  p_rc      REFCURSOR,
  INOUT  p_error_message   VARCHAR)
  AS $$
  DECLARE 
		v_user_id   NUMERIC;
    --UNKNOWN_USER_EXCEP   EXCEPTION := 'UNKNOWN_USER_EXCEP';
	BEGIN
    v_user_id := UserIdFromCode(p_user_code);

    IF v_user_id = -1 THEN
      RAISE EXCEPTION 'UNKNOWN_USER_EXCEP' USING errcode='50001';
    END IF;

    OPEN p_rc FOR
      WITH RECURSIVE tmp AS (SELECT   user_id, object_id, object_name, object_desc, object_prog_name, assembly_name, parent_object_id, object_type_desc,
                 object_text, object_icon_key, is_active, is_node, help_topic_id, display_order,
				 CASE WHEN hasdelete = 0 THEN has_delete = 0 ELSE has_delete = 1 END,
				 CASE WHEN hasupdate = 0 THEN has_update = 0 ELSE has_update = 1 END,
				 CASE WHEN hasread = 0 THEN has_read = 0 ELSE has_read = 1 END,
				 CASE WHEN hascreate = 0 THEN has_create = 0 ELSE has_create = 1 END, LEVEL
                 --DECODE(hasdelete, 0, 0, 1) has_delete, DECODE(hasupdate, 0, 0, 1) has_update,
                 --DECODE(hasread, 0, 0, 1) has_read, DECODE(hascreate, 0, 0, 1) has_create, LEVEL
            FROM (SELECT   grpa.user_id, obj.object_id, obj.object_name, obj.object_desc, obj.object_prog_name, obj.assembly_name,
                           obj.parent_object_id, objt.object_type_desc, obj.object_text, obj.object_icon_key,
                           obj.is_active, obj.is_node, COALESCE(obj.help_topic_id, 0) help_topic_id, obj.display_order,
                           SUM((gop.access_rights & 8)) hasdelete, SUM((gop.access_rights & 4)) hasupdate,
                           SUM((gop.access_rights & 2)) hasread, SUM((gop.access_rights & 1)) hascreate
                      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop, sec_object_new obj,
                           sec_object_type_new objt, sec_user_new usr
                     WHERE grpa.user_group_id = gop.user_group_id
                       AND gop.object_id = obj.object_id
                       AND obj.object_type_id = objt.object_type_id
                       AND grpa.user_id = usr.user_id
                       AND DATE_TRUNC('day', CURRENT_DATE) BETWEEN DATE_TRUNC('day', usr.effective_date) AND DATE_TRUNC('day',COALESCE(usr.end_date, CURRENT_DATE + 1))
                       AND usr.user_id = v_user_id
                  GROUP BY grpa.user_id, obj.object_id, obj.object_name, obj.object_desc, obj.object_prog_name, obj.assembly_name,
                           obj.parent_object_id, objt.object_type_desc, obj.object_text, obj.object_icon_key,
                           obj.is_active, obj.is_node, obj.help_topic_id, obj.display_order) AS query
                 WHERE parent_object_id IS NULL
           UNION
      SELECT user_id, object_id, object_name, object_desc, object_prog_name, assembly_name, parent_object_id, object_type_desc,
                 object_text, object_icon_key, is_active, is_node, help_topic_id, display_order,
				 CASE WHEN hasdelete = 0 THEN has_delete = 0 ELSE has_delete = 1 END,
				 CASE WHEN hasupdate = 0 THEN has_update = 0 ELSE has_update = 1 END,
				 CASE WHEN hasread = 0 THEN has_read = 0 ELSE has_read = 1 END,
				 CASE WHEN hascreate = 0 THEN has_create = 0 ELSE has_create = 1 END, LEVEL
                 --DECODE(hasdelete, 0, 0, 1) has_delete, DECODE(hasupdate, 0, 0, 1) has_update,
                 --DECODE(hasread, 0, 0, 1) has_read, DECODE(hascreate, 0, 0, 1) has_create, LEVEL
            FROM (SELECT   grpa.user_id, obj.object_id, obj.object_name, obj.object_desc, obj.object_prog_name, obj.assembly_name,
                           obj.parent_object_id, objt.object_type_desc, obj.object_text, obj.object_icon_key,
                           obj.is_active, obj.is_node, COALESCE(obj.help_topic_id, 0) help_topic_id, obj.display_order,
                           SUM((gop.access_rights & 8)) hasdelete, SUM((gop.access_rights & 4)) hasupdate,
                           SUM((gop.access_rights & 2)) hasread, SUM((gop.access_rights & 1)) hascreate
                      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop, sec_object_new obj,
                           sec_object_type_new objt, sec_user_new usr
                     WHERE grpa.user_group_id = gop.user_group_id
                       AND gop.object_id = obj.object_id
                       AND obj.object_type_id = objt.object_type_id
                       AND grpa.user_id = usr.user_id
                       AND DATE_TRUNC('day', CURRENT_DATE) BETWEEN DATE_TRUNC('day', usr.effective_date) AND DATE_TRUNC('day', COALESCE(usr.end_date, CURRENT_DATE + 1))
                       AND usr.user_id = v_user_id
                  GROUP BY grpa.user_id, obj.object_id, obj.object_name, obj.object_desc, obj.object_prog_name, obj.assembly_name,
                           obj.parent_object_id, objt.object_type_desc, obj.object_text, obj.object_icon_key,
                           obj.is_active, obj.is_node, obj.help_topic_id, obj.display_order) AS query
      JOIN tmp ON tmp.parent_object_id = object_id AND tmp.user_id = user_id)
		SELECT user_id, object_id, object_name, object_desc, object_prog_name, assembly_name, parent_object_id, object_type_desc,
                 object_text, object_icon_key, is_active, is_node, help_topic_id, display_order,
				 CASE WHEN hasdelete = 0 THEN has_delete = 0 ELSE has_delete = 1 END,
				 CASE WHEN hasupdate = 0 THEN has_update = 0 ELSE has_update = 1 END,
				 CASE WHEN hasread = 0 THEN has_read = 0 ELSE has_read = 1 END,
				 CASE WHEN hascreate = 0 THEN has_create = 0 ELSE has_create = 1 END, LEVEL
                 --DECODE(hasdelete, 0, 0, 1) has_delete, DECODE(hasupdate, 0, 0, 1) has_update,
                 --DECODE(hasread, 0, 0, 1) has_read, DECODE(hascreate, 0, 0, 1) has_create, LEVEL 
				 FROM tmp
				 ORDER BY tmp.user_id, tmp.display_order;
  EXCEPTION  when SQLSTATE '50001' then
    --WHEN UNKNOWN_USER_EXCEP THEN
      p_error_message := 'Provided user does not exist';
    WHEN OTHERS THEN
      IF SQLERRM IS NOT NULL THEN
        p_error_message := p_error_message || ' ' || SQLERRM;
      END IF;
  END ; 
  $$
  LANGUAGE plpgsql ;
  
  
  
 CREATE OR REPLACE FUNCTION GetBBObjectGroup(
    p_object_name     VARCHAR,
   OUT p_rc     refcursor,
   INOUT p_error_message    VARCHAR)
  AS $$
  DECLARE v_errholder VARCHAR(255);
	BEGIN
    OPEN p_rc FOR
      SELECT grp.user_group_name
        FROM sec_object_new obj, sec_group_object_priv_new gop, sec_group_new grp
       WHERE obj.object_id = gop.object_id
         AND gop.user_group_id = grp.user_group_id
         AND obj.object_name = 'MSO Form'
         AND gop.access_rights = 15;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_error_message := SQLERRM;
  END;
  $$
  LANGUAGE plpgsql ;
  
  
  
  
  CREATE OR REPLACE FUNCTION BlobToBinary(
    p_blob     VARCHAR,
 OUT   p_result    VARCHAR,
  OUT  p_ListLen    NUMERIC,
 p_binarylist VARCHAR)
    RETURNS record
  AS $$
  DECLARE 
	v_blob    VARCHAR(32000);
    i    NUMERIC(10) := 1;
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
END;
  $$
  LANGUAGE plpgsql ;
  
  
  
 /*  
CREATE OR REPLACE FUNCTION BlobToBinary(
    p_blob        VARCHAR,
  OUT  p_result    VARCHAR,
  OUT  p_ListLen  NUMERIC,
   OUT p_BinaryList   t_IdRec)
  AS $$
  DECLARE 
  v_blob                                      VARCHAR(32000);
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
    END IF; *//*
    p_result := '';
    p_ListLen := i - 1;
  EXCEPTION
    WHEN OTHERS THEN
      p_result := SQLERRM;
  END ;
  $$
  LANGUAGE plpgsql ;
   */
  
  
  CREATE OR REPLACE FUNCTION IsUserInGroup(
    p_user_code                    VARCHAR,
    p_group_name             VARCHAR)
  RETURNS NUMERIC AS $$
  DECLARE 
	v_count         NUMERIC;
  BEGIN
    SELECT COUNT(1)
      INTO v_count
      FROM sec_user_new usr, sec_group_assignment_new grpa, sec_group_new grp
     WHERE usr.user_id = grpa.user_id
       AND grpa.user_group_id = grp.user_group_id
       AND usr.user_code = p_user_code
       AND grp.user_group_name = p_group_name;

    RETURN v_count;
  END ;  $$
LANGUAGE plpgsql;
  
  
  
  
   CREATE OR REPLACE FUNCTION UserHasCreateForObject(
    p_user_id             NUMERIC,
    p_object_id          NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_hascreate                                 NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 1)), 0)
      INTO v_hascreate
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_hascreate != 0 THEN
      v_hascreate := 1;
    END IF;

    RETURN v_hascreate;
  END ;  $$ -- End of Function UserHasCreateForObject
LANGUAGE plpgsql;
  
  
  
  CREATE OR REPLACE FUNCTION UserHasReadForObject(
    p_user_id                NUMERIC,
    p_object_id             NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 2)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ; $$  -- End of Function UserHasReadForObject
LANGUAGE plpgsql;
  
  
  
   CREATE OR REPLACE FUNCTION UserHasUpdateForObject(
    p_user_id              NUMERIC,
    p_object_id          NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 4)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ; $$  -- End of Function UserHasUpdateForObject
 LANGUAGE plpgsql;
 
 

 CREATE OR REPLACE FUNCTION UserHasUpdateOnObject(
    p_user_code                  VARCHAR,
    p_object_id                   NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
    v_user_id                                   NUMERIC;
      
  BEGIN
    v_user_id := UserIdFromCode(p_user_code);
  
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 4)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = v_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
    
  END ; $$  -- End of Function UserHasUpdateOnObject
LANGUAGE plpgsql;

  CREATE OR REPLACE FUNCTION UserHasDeleteForObject(
    p_user_id                  NUMERIC,
    p_object_id               NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 8)), 0)
      INTO v_has
      FROM sec_group_assignment_new grpa, sec_group_object_priv_new gop
     WHERE grpa.user_group_id = gop.user_group_id AND grpa.user_id = p_user_id AND gop.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ; $$  -- End of Function UserHasDeleteForObject
LANGUAGE plpgsql;



  CREATE OR REPLACE FUNCTION ObjectHasCreateForGroup(
    p_object_id            NUMERIC,
    p_group_id                NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 1)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ;  $$  -- End of Function ObjectHasCreateForGroup
LANGUAGE plpgsql;



  CREATE OR REPLACE FUNCTION ObjectHasReadForGroup(
    p_object_id              NUMERIC,
    p_group_id               NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 2)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ; $$  -- End of Function ObjectHasReadForGroup
LANGUAGE plpgsql;



  CREATE OR REPLACE FUNCTION ObjectHasUpdateForGroup(
    p_object_id           NUMERIC,
    p_group_id                NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 4)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ; $$   -- End of Function ObjectHasUpdateForGroup
LANGUAGE plpgsql;



  CREATE OR REPLACE FUNCTION ObjectHasDeleteForGroup(
    p_object_id             NUMERIC,
    p_group_id             NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_has                                       NUMERIC;
  BEGIN
    SELECT COALESCE(SUM((cast(gop.access_rights as integer) & 8)), 0)
      INTO v_has
      FROM sec_object_new obj, sec_group_object_priv_new gop
     WHERE obj.object_id = gop.object_id AND gop.user_group_id = p_group_id AND obj.object_id = p_object_id;

    IF v_has != 0 THEN
      v_has := 1;
    END IF;

    RETURN v_has;
  END ;  $$  -- End of Function ObjectHasDeleteForGroup
LANGUAGE plpgsql;



  --Purpose(GetObjectParentHierarchyName):
  --        Returns the parent hierarchy information
  CREATE OR REPLACE FUNCTION GetObjectParentHierarchyName(
    p_object_id         NUMERIC)
    RETURNS VARCHAR AS $$
	DECLARE
     cr_hierarchy CURSOR(c_object_id  NUMERIC) FOR 
	 WITH RECURSIVE tmp AS (SELECT obj.object_id, COALESCE(obj.parent_object_id, 0) parent_object_id, obj.object_text
            FROM sec_object_new obj
           WHERE obj.object_id = c_object_id
           UNION
      SELECT obj.object_id, COALESCE(obj.parent_object_id, 0) parent_object_id, obj.object_text FROM sec_object_new obj
      JOIN tmp ON tmp.parent_object_id = obj.object_id)
		SELECT object_id, COALESCE(parent_object_id, 0) parent_object_id, object_text FROM tmp;

    v_hierarchy                                 VARCHAR(1000) := '';
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
  END ; $$
LANGUAGE plpgsql;



 CREATE OR REPLACE FUNCTION GetObjectSortLevel(
    p_object_id              NUMERIC)
    RETURNS NUMERIC AS $$
	DECLARE
    v_sort_level                                NUMERIC;
  BEGIN
     SELECT SUM(sort_order)
     INTO v_sort_level
      FROM (WITH RECURSIVE tmp (object_id,parent_object_id,sort_order)
    AS (SELECT object_id,parent_object_id,(POWER(10, objtyp.level_number) * obj.display_order ) as sort_order
          FROM sec_object_new obj, sec_object_type_new objtyp
                 WHERE obj.object_type_id = objtyp.object_type_id
         AND object_id = 20
        UNION ALL
        SELECT t_2.object_id,t_2.parent_object_id,(POWER(10, objtyp.level_number) * t_2.display_order ) as sort_order
          FROM sec_object_new AS t_2, tmp, sec_object_type_new objtyp
                 WHERE t_2.object_type_id = objtyp.object_type_id
         AND tmp.parent_object_id = t_2.object_id)
    SELECT object_id,parent_object_id,sort_order
      FROM tmp) as a ;

    RETURN v_sort_level;
  END ; $$
LANGUAGE plpgsql;


  
  
    CREATE OR REPLACE FUNCTION UserIdFromCode(
    p_user_code              VARCHAR)
    RETURNS NUMERIC AS $$
	DECLARE
    v_user_id                                   NUMERIC;
  BEGIN
    SELECT user_id
      INTO v_user_id
      FROM sec_user_new
     WHERE user_code = p_user_code;

    RETURN v_user_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      RETURN -1;
  END ; $$
 LANGUAGE plpgsql;
END;  

 -- End of DDL Script for Package Body APPSECURITY_NEW
/
  