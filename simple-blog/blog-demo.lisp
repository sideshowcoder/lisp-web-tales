(ql:quickload '(:restas :sexml))

(restas:define-module #:blog-demo
  (:use #:cl #:restas))

(in-package #:blog-demo)

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))

(<:augment-with-doctype "html" "")

(defparameter *posts* nil)

(defun slug (string)
  (substitute #\- #\Space
              (string-downcase
               (string-trim '(#\Space #\Tab #\Newline)
                            string))))

(defun html-frame (title body)
  (<:html
   (<:head (<:title title))
   (<:body
    (<:a :href (genurl 'home) (<:h1 title))
    body)))

(defun render-post (post)
  (list (<:div
         (<:h2 (<:a
                :href (genurl 'post :id (position post *posts* :test #'equal))
                (getf post :title)))
         (<:h3 (<:a
                :href (genurl 'author :id (getf post :author-id))
                "By " (getf post :author)))
         (<:p (getf post :content)))
        (<:hr)))

(defun render-posts (posts)
  (mapcar #'render-post posts))

(defun blogpage (&optional (posts *posts*))
  (html-frame
   "Restas Blog demo"
   (<:div
    (<:a :href (genurl 'add) "Create a new posts")
    (<:hr)
    (render-posts posts))))


(defun add-post-form ()
  (html-frame
   "Restas Blog demo"
   (<:form :action (genurl 'add/post) :method "post"
           "Author name:" (<:br)
           (<:input :type "text" :name "author") (<:br)
           "Title:" (<:br)
           (<:input :type "text" :name "title") (<:br)
           "Content:" (<:br)
           (<:textarea :name "content" :rows 15 :cols 80) (<:br)
           (<:input :type "submit" :value "Submit"))))
           
(define-route home ("")
  (blogpage))

(define-route post ("post/:id")
  (let* ((id (parse-integer id :junk-allowed t))
         (post (elt *posts* id)))
    (blogpage (list post))))

(define-route author ("author/:id")
  (let ((posts (loop for post in *posts*
                  if (equal id (getf post :author-id))
                  collect post)))
    (blogpage posts)))

(define-route add ("add")
  (multiple-value-bind (username password) (hunchentoot:authorization)
    (if (and (equalp username "user")
             (equalp password "password"))
        (add-post-form)
        (hunchentoot:require-authorization))))

(define-route add/post ("add" :method :post)
  (let ((author (hunchentoot:post-parameter "author"))
        (title (hunchentoot:post-parameter "title"))
        (content (hunchentoot:post-parameter "content")))
    (push (list :author author
                :author-id (slug author)
                :title title
                :content content) *posts*)
    (redirect 'home)))

(start '#:blog-demo :port 4711)
                         
        
             
    
           
