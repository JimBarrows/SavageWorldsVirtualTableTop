<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://javadomainobjects.org/tags/ember" prefix="ember"%>
<div class="container">
	<div class="page-header">
		<h1>Create Edge Type</h1>
	</div>
	<form class="form-horizontal" role="form">
	
		<ember:TextField label="Name" field="name" placeHolder="Name"/>		
		
		<ember:TextAreaField label="Description" field="description" />

		<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
	</form>
</div>