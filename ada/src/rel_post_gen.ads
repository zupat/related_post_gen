with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;
with Ada.Containers.Indefinite_Hashed_Maps;

package Rel_Post_Gen is
   Top_N     : constant Natural := 5;
   Max_Posts : constant Natural := 60000;
   type Top_N_Range is range 0 .. Top_N - 1;
   type Count is range 0 .. Max_Posts;
   type Index is range 0 .. Max_Posts;

   package JSON renames GNATCOLL.JSON;
   package UStr renames Ada.Strings.Unbounded;
   package TIO renames Ada.Text_IO;
   package UTF8 renames Ada.Strings.UTF_Encoding;

   subtype Tag is UStr.Unbounded_String;
   package Tag_Vectors is new
     Ada.Containers.Vectors
       (Element_Type => Tag,
        Index_Type   => Index,
        "="          => UStr."=");

   type Post is record
      Id    : UStr.Unbounded_String;
      Title : UStr.Unbounded_String;
      Tags  : Tag_Vectors.Vector;
   end record;
   type Post_Acc is access Post;

   package Post_Acc_Vectors is new
     Ada.Containers.Vectors (Element_Type => Post_Acc, Index_Type => Index);
   package Index_Vectors is new
     Ada.Containers.Vectors (Element_Type => Index, Index_Type => Index);
   type Index_Vectors_Acc is access Index_Vectors.Vector;

   package Tag_To_Index_Vector_Map is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Tag,
        Element_Type    => Index_Vectors_Acc,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => UStr."=");

   type Top_N_Related_Posts is array (Top_N_Range) of Post_Acc;
   type Related_Posts_Rec is record
      Id      : UStr.Unbounded_String;
      Tags    : Tag_Vectors.Vector;
      Related : Top_N_Related_Posts;
   end record;
   type Related_Posts_Acc is access Related_Posts_Rec;
   type Related_Posts_Array is array (Index range <>) of Related_Posts_Acc;

   type Tagged_Post_Count_Array is array (Index range <>) of Count;
   type Top_N_Count_Array is array (Top_N_Range) of Count;
   type Top_N_Indexes_Array is array (Top_N_Range) of Index;

   function Get_Posts
     (Path : UTF8.UTF_8_String) return Post_Acc_Vectors.Vector;
   function Get_Related
     (Flat_Posts : Post_Acc_Vectors.Vector) return Related_Posts_Array;
   function Encode_To_JSON (Related_Posts : Related_Posts_Array) return String;

end Rel_Post_Gen;
