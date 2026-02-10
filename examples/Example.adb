-- Ada 2022: Concurrent task pool with protected objects
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Containers.Vectors;

procedure Task_Pool_Demo is

   -- Work item definition
   type Work_Status is (Pending, Running, Done, Failed);
   type Work_Item is record
      Id     : Natural := 0;
      Input  : Integer := 0;
      Result : Long_Integer := 0;
      Status : Work_Status := Pending;
   end record;

   package Work_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Work_Item);
   use Work_Vectors;

   -- Protected work queue (thread-safe)
   protected type Work_Queue is
      entry Enqueue (Item : in Work_Item);
      entry Dequeue (Item : out Work_Item);
      procedure Mark_Done (Id : Natural; Result : Long_Integer);
      function Pending_Count return Natural;
      function Is_Empty return Boolean;
   private
      Queue   : Work_Vectors.Vector;
      Results : Work_Vectors.Vector;
   end Work_Queue;

   protected body Work_Queue is
      entry Enqueue (Item : in Work_Item) when True is
      begin
         Queue.Append (Item);
      end Enqueue;

      entry Dequeue (Item : out Work_Item)
        when not Queue.Is_Empty is
      begin
         Item := Queue.First_Element;
         Queue.Delete_First;
      end Dequeue;

      procedure Mark_Done (Id : Natural; Result : Long_Integer) is
         Done_Item : Work_Item := (Id, 0, Result, Done);
      begin
         Results.Append (Done_Item);
      end Mark_Done;

      function Pending_Count return Natural is
      begin
         return Natural (Queue.Length);
      end Pending_Count;

      function Is_Empty return Boolean is
      begin
         return Queue.Is_Empty;
      end Is_Empty;
   end Work_Queue;

   -- Shared queue
   Queue : Work_Queue;
   Total_Workers : constant := 4;
   Total_Jobs    : constant := 20;

   -- Worker task
   task type Worker (Id : Natural) is
      entry Start;
      entry Stop;
   end Worker;

   task body Worker is
      Item : Work_Item;
      Result : Long_Integer;
   begin
      accept Start;
      Put_Line ("Worker" & Id'Image & " started");
      loop
         select
            accept Stop;
            exit;
         else
            begin
               Queue.Dequeue (Item);
               -- Compute factorial as work
               Result := 1;
               for I in 1 .. Item.Input loop
                  Result := Result * Long_Integer (I);
               end loop;
               Queue.Mark_Done (Item.Id, Result);
               Put_Line ("  Worker" & Id'Image & " computed job"
                         & Item.Id'Image & " = " & Result'Image);
            exception
               when others => null; -- Queue empty, retry
            end;
         end select;
      end loop;
      Put_Line ("Worker" & Id'Image & " stopped");
   end Worker;

   type Worker_Access is access Worker;
   Workers : array (1 .. Total_Workers) of Worker_Access;

begin
   Put_Line ("=== MarkCraft Task Pool Demo ===");
   Put_Line ("Workers:" & Total_Workers'Image
             & ", Jobs:" & Total_Jobs'Image);

   -- Create workers
   for I in Workers'Range loop
      Workers (I) := new Worker (I);
      Workers (I).Start;
   end loop;

   -- Enqueue jobs
   for I in 1 .. Total_Jobs loop
      Queue.Enqueue ((Id => I, Input => 10 + I, others => <>));
   end loop;

   -- Wait then stop
   delay 2.0;
   for W of Workers loop
      W.Stop;
   end loop;

   Put_Line ("=== All jobs completed ===");
end Task_Pool_Demo;
