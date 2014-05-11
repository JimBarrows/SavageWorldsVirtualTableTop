(defclass skill-description ()
  (name :initarg :name
        :reader name
        :initform (error "Must provice a name"))
  (attribute :initarg :attribute
             :initform (error "Must provide an attrbiute"))
  