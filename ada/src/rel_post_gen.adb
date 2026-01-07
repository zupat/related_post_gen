with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Rel_Post_Gen is
   Top_N: constant Natural := 5;
   Max_Posts: constant Natural := 60000;
   type Top_N_Range is range 0 .. Top_N-1;
   type Count is range 0 .. Max_Posts;
   type Index is range 0 .. Max_Posts;

   package JSON renames GNATCOLL.JSON;
   package UStr renames Ada.Strings.Unbounded;
   package TIO renames Ada.Text_IO;
   package UTF8 renames Ada.Strings.UTF_Encoding;

   subtype Tag is UStr.Unbounded_String;
   package Tag_Vectors is new Ada.Containers.Vectors
     (Element_Type => Tag,
      Index_Type   => Index,
      "="          => UStr."=");

   type Post is record
      Id: UStr.Unbounded_String;
      Title: UStr.Unbounded_String;
      Tags: Tag_Vectors.Vector;
   end record;
   type Post_Acc is access Post;

   package Post_Acc_Vectors is new Ada.Containers.Vectors
     (Element_Type => Post_Acc,
      Index_Type   => Index);
   package Index_Vectors is new Ada.Containers.Vectors
     (Element_Type => Index,
      Index_Type   => Index);
   type Index_Vectors_Acc is access Index_Vectors.Vector;

   package Tag_To_Index_Vector_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Tag,
      Element_Type    => Index_Vectors_Acc,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => UStr."=");

   type Top_N_Related_Posts is array (Top_N_Range) of Post_Acc;
   type Related_Posts_Rec is record
      Id: UStr.Unbounded_String;
      Tags: Tag_Vectors.Vector;
      Related: Top_N_Related_Posts;
   end record;
   type Related_Posts_Acc is access Related_Posts_Rec;
   type Related_Posts_Array is array (Index range <>) of Related_Posts_Acc;

   -- --------------------------------------------------------------------------------
   function Get_Posts (Path: UTF8.UTF_8_String) return Post_Acc_Vectors.Vector is
      Read_Result: JSON.Read_Result;
      File_Content: JSON.JSON_Array;
      Read_Json_Err: exception;
      Result: Post_Acc_Vectors.Vector;
   begin
      Read_Result := JSON.Read_File (Path);
      if not Read_Result.Success  then
         raise Read_Json_Err with "on JSON.Read_File: " & Read_Result.Error'Image;
      end if;

      case Read_Result.Value.Kind is
         when JSON.JSON_Array_Type =>
            File_Content := Read_Result.Value.Get;
         when others =>
            raise Read_Json_Err with
              "Expected json data type JSON_ARRAY_TYPE, got " & Read_Result.Value.Kind'Image;
      end case;

      for Obj of File_Content loop
         declare
            P: Post_Acc :=
              new Post'(Id => Obj.Get("_id"),
                        Title => Obj.Get("title"),
                        Tags => Tag_Vectors.Empty_Vector);
         begin
            for T of JSON.JSON_Array'(Obj.Get("tags")) loop
               P.Tags.Append(JSON.Get(T));
            end loop;
            Result.Append(P);
         end;
      end loop;

      return Result;
   end Get_Posts;

   -- --------------------------------------------------------------
   Start_Time, Stop_Time : Ada.Real_Time.Time;
   type Tagged_Post_Count_Array is array (Index range <>) of Count;
   type Top_N_Count_Array is array (Top_N_Range) of Count;
   type Top_N_Indexes_Array is array (Top_N_Range) of Index;

   Result: JSON.JSON_Array := JSON.Empty_Array;
   Flat_Posts: Post_Acc_Vectors.Vector := Get_Posts ("../posts.json");
   Num_Posts: Index := Index(Flat_Posts.Length);
   Tag_Map: Tag_To_Index_Vector_Map.Map;
   Tagged_Post_Count: Tagged_Post_Count_Array(0 .. Num_Posts-1);
   Related_Posts: Related_Posts_Array (0 .. Num_Posts-1);
begin
   Start_Time := Clock;

   for Post_Idx in Flat_Posts.First_Index .. Flat_Posts.Last_Index loop
      for Tag of Flat_Posts(Post_Idx).Tags loop
         declare
            Vec: Index_Vectors_Acc;
         begin
            if Tag_Map.Contains(Tag) then
               Vec := Tag_Map(Tag);
            else
               Vec := new Index_Vectors.Vector;
               Tag_Map.Include (Tag, Vec);
            end if;
            Vec.Append(Post_Idx);
         end;
      end loop;
   end loop;

   for Post_Idx in Flat_Posts.First_Index .. Flat_Posts.Last_Index loop
      Tagged_Post_Count := (others => 0);
      for Tag of Flat_Posts(Post_Idx).Tags loop
         for Tagged_Post_Idx of Tag_Map(Tag).all loop
            Tagged_Post_Count(Tagged_Post_Idx) := @ + 1;
         end loop;
      end loop;
      Tagged_Post_Count(Post_Idx) := 0; -- don't count self

      declare
         Top_N_Indexes: Top_N_Indexes_Array := (others => 0);
         Top_N_Count: Top_N_Count_Array := (others => 0);
         Top_N_Posts: Top_N_Related_Posts;
         Related_Posts_Count: Count := 0;
         -- Pos: Index;
      begin
         for Related_Post_Idx in Tagged_Post_Count'Range loop
            Related_Posts_Count := Tagged_Post_Count(Related_Post_Idx);
            if Related_Posts_Count > Top_N_Count(Top_N_Count'Last) then
               for Pos in Top_N_Range loop
                  if Top_N_Count(Pos) < Related_Posts_Count then
                     if Pos < Top_N_Range'Last then
                        Top_N_Indexes(Pos+1 .. Top_N_Range'Last) := Top_N_Indexes(Pos .. Top_N_Range'Last-1);
                     end if;
                     Top_N_Indexes(Pos) := Related_Post_Idx;
                     Top_N_Count(Pos) := Related_Posts_Count;
                     exit;
                  end if;
               end loop;
            end if;
         end loop;

         for I in Top_N_Indexes'Range loop
            Top_N_Posts(I) := Flat_Posts(Top_N_Indexes(I));
         end loop;

         Related_Posts(Post_Idx) := new
           Related_Posts_Rec'(Id => Flat_Posts(Post_Idx).Id,
                              Tags =>Flat_Posts(Post_Idx).Tags,
                              Related => Top_N_Posts);
      end;
   end loop;

   Stop_Time := Clock;

   -- Assamble  JSON
   for P of Related_Posts loop
      declare
         Rec: JSON.JSON_Value := JSON.Create_Object;
         Related, Tags: JSON.JSON_Array := JSON.Empty_Array;
      begin
         for Tag of P.Tags loop
            JSON.Append(Tags, JSON.Create(Tag));
         end loop;
         for Post of P.Related loop
            declare
               JSON_Post: JSON.JSON_Value := JSON.Create_Object;
               Related_Tags: JSON.JSON_Array := JSON.Empty_Array;
            begin
               for Tag of Post.Tags loop
                  JSON.Append(Related_Tags, JSON.Create(Tag));
               end loop;
               JSON.Set_Field(JSON_Post, "_id", Post.Id);
               JSON.Set_Field(JSON_Post, "title", Post.Title);
               JSON.Set_Field(JSON_Post, "tags", Related_Tags);
               JSON.Append(Related, JSON_Post);
            end;
         end loop;

         JSON.Set_Field(Rec, "_id", P.Id);
         JSON.Set_Field(Rec, "tags", Tags);
         JSON.Set_Field(Rec, "related", Related);
         JSON.Append(Result, Rec);
      end;
   end loop;


   TIO.Put_Line ("Processing time (w/o IO): "
                 & Duration'Image (To_Duration(Stop_Time - Start_Time) * 1000)
                 & "ms");

   -- Write JSON to file
   declare
      F: TIO.File_Type;
      Out_File_Name: constant String := "../related_posts_ada.json";
   begin
      TIO.Create(F, TIO.Out_File, Out_File_Name);
      TIO.Put(F, JSON.Write(JSON.Create(Result)));
      TIO.Close (F);
   end;

end Rel_Post_Gen;
