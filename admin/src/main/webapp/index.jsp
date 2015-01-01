<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Savage Worlds - Admin</title>
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="">
<meta name="author" content="">
<link rel="icon" href="../../favicon.ico">
<link rel="stylesheet" href="css/normalize.css">
<!-- Latest compiled and minified CSS -->
<link rel="stylesheet"
	href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css">

<!-- Optional theme -->
<link rel="stylesheet"
	href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap-theme.min.css">
<link rel="stylesheet" href="css/style.css">
</head>
<body role="document">
	<script type="text/x-handlebars">
		<%@ include file="template.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="index">
		<div class="page-header">
			<h1>Administration</h1>
		</div>
	</script>

	<script type="text/x-handlebars" id="components/field-editor">
    	<textarea class="form-control">{{value}}</textarea>
  	</script>

	<script type="text/x-handlebars" id="skilldescriptions/index">
		<%@ include file="skilldescriptions/index.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="skilldescriptions/create">
		<%@ include file="skilldescriptions/create.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="skilldescriptions/edit">
		<%@ include file="skilldescriptions/edit.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="armordescriptions/index">
		<%@ include file="armordescriptions/index.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="armordescriptions/create">
		<%@ include file="armordescriptions/create.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="armordescriptions/edit">
		<%@ include file="armordescriptions/edit.jsp"%>		
	</script>
	
	<script type="text/x-handlebars" id="edgetypes/index">
		<%@ include file="edgetypes/index.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="edgetypes/create">
		<%@ include file="edgetypes/create.jsp"%>		
	</script>

	<script type="text/x-handlebars" id="edgetypes/edit">
		<%@ include file="edgetypes/edit.jsp"%>		
	</script>

	<script
		src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
	<script src="js/libs/handlebars-v2.0.0.js"></script>
	<script src="js/libs/ember-1.9.0.js"></script>
	<script src="js/libs/ember-data-1.0.0-beta.11.js"></script>
	<script
		src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js"></script>
	<script src="//cdn.ckeditor.com/4.4.6/full-all/ckeditor.js"></script>
	<script src="//cdn-source.ckeditor.com/4.4.6/full-all/adapters/jquery.js"></script>
	<script src="js/app.js"></script>
	<script src="js/models.js"></script>
	<script src="js/controllers.js"></script>
	<script src="js/skillDescription.js"></script>
	<script src="js/armorDescription.js"></script>
	<script src="js/edgeType.js"></script>
	<script src="js/routes.js"></script>


</body>
</html>
