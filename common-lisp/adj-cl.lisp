(declaim (optimize (speed 3) (safety 1)))

(ql:quickload '(:com.inuoe.jzon :lla))

(in-package :cl-user)

(defparameter *top-n* 5)

(defstruct post
  (id nil :type simple-string)
  (tags nil :type simple-vector)
  (title nil :type simple-string))

(defun make-tag-map (posts)
  ;; hashtable of tags->posts
  (let ((tag-table (make-hash-table :test #'equal :size 10000)))
    (loop for idx from 0
          for p across posts
          do (loop for tag across (post-tags p)
                   do (push idx (gethash tag tag-table))))
    tag-table))

(defun make-bipartite-graph (tag->post number-of-posts)
  ;; returns the adjacency-matrix tags x posts
  (let* ((tag-vector (coerce (loop for key being each hash-key of tag->post collect key) 'vector))
         (num-tags (length tag-vector))
         (adjacency-array (make-array (list num-tags number-of-posts)
                                      :element-type 'double-float
                                      :initial-element 0d0)))
    (loop for tag across tag-vector
          for tag-idx from 0
          do (loop for post-idx in (gethash tag tag->post)
                   do (setf (aref adjacency-array tag-idx post-idx) 1d0)))
    adjacency-array))

(defun compute-top-n (projection post-idx n)
  ;; gets the top N related counts for a post
  (declare (type fixnum post-idx n))
  (let* ((num-posts (array-dimension projection 1))
         (top (make-array n :initial-element (cons -1 0d0))))

    (setf (aref projection post-idx post-idx) 0d0)
    
    (loop for j fixnum from 0 below num-posts
          for count double-float = (aref projection post-idx j)
          when (> count (cdr (aref top 0)))
            do (setf (aref top 0) (cons j count))
               (sort top #'< :key #'cdr))
    
    (mapcar #'car 
            (remove-if (lambda (x) (= (car x) -1))
                       (sort (coerce top 'list) #'> :key #'cdr)))))

(defun build-output (posts projection)
  ;; prints the json
  (let ((output (make-hash-table :test #'equal))
        (num-posts (length posts)))
    (loop for post-idx from 0 below num-posts
          for post = (aref posts post-idx)
          do (setf (aref projection post-idx post-idx) 0d0)
             (let ((counts (loop for j from 0 below num-posts
                                 collect (aref projection post-idx j))))
               (setf counts (sort counts #'>))
               (setf (gethash (post-id post) output)
                     (round (reduce #'+ (subseq counts 0 *top-n*))))))
    output))

(defun now ()
  (float (/ (get-internal-real-time)
            internal-time-units-per-second)))

(defun main ()
  (let* ((raw-posts (map 'vector
                         (lambda (p)
                           (make-post :id (gethash "_id" p)
                                      :tags (gethash "tags" p)
                                      :title (gethash "title" p)))
                         (the simple-vector
                              (com.inuoe.jzon:parse #p"../posts.json"))))
         (T1 (now))
         (num-posts (length raw-posts))
         (tag-map (make-tag-map raw-posts))
         (bp-graph (make-bipartite-graph tag-map num-posts))
         ;; A^T × A = (posts × tags) × (tags × posts) = (posts × posts)
         (projection (lla:mm (nu:transpose bp-graph) bp-graph))
         (T2 (now))
         (output (build-output raw-posts projection)))
    
    (with-open-file (out #p"../related_posts_cl.json"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (com.inuoe.jzon:stringify output :stream out))
    
    (format t "Processed ~d posts~%" num-posts)
    (format t "Processing time (w/o IO): ~2$ s~%" (- T2 T1))))

(save-lisp-and-die "related" :toplevel #'main :executable t :save-runtime-options t)

