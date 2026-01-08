with Ada.Real_Time; use Ada.Real_Time;
with Rel_Post_Gen;  use Rel_Post_Gen;

procedure Main is
   Start_Time, Stop_Time : Ada.Real_Time.Time;
   Posts                 : constant Post_Acc_Vectors.Vector := Get_Posts ("../posts.json");
   Out_File              : TIO.File_Type;
   Num_Posts             : constant Index := Index (Posts.Length);
   Related_Posts         : Related_Posts_Array (0 .. Num_Posts - 1);
begin
   Start_Time := Clock;
   Related_Posts := Get_Related (Posts);
   Stop_Time := Clock;

   TIO.Put_Line
     ("Processing time (w/o IO): "
      & Duration'Image (To_Duration (Stop_Time - Start_Time) * 1000)
      & "ms");

   TIO.Create (Out_File, TIO.Out_File, "../related_posts_ada.json");
   TIO.Put (Out_File, Encode_To_JSON (Related_Posts));
   TIO.Close (Out_File);
end Main;
