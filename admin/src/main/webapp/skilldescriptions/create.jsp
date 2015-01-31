<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://javadomainobjects.org/tags/ember" prefix="ember"%>
<div class="container">
	<div class="page-header">
		<h1>Create Skill</h1>
	</div>
	<form class="form-horizontal" role="form">
	
		<ember:TextField label="Name" field="name" placeHolder="Skill Name"/>

		<div {{bind-attr class=":form-group model.errors.attribute:has-error"}}>
			<label for="attribute" class="col-sm-2 control-label">Attribute</label>
			<div class="col-sm-10">
				{{view "select" content=attributeTypes selection=model.attribute class="form-control" prompt="Please select an attribute"}} 				
				{{#if model.errors.attribute}} 
					<span class="glyphicon glyphicon-remove form-control-feedback"></span> 
					<span class="help-block">
						<ul>
							<li>{{#each model.errors.attribute}} {{message}} {{/each}}</li>
						</ul>
					</span> 
				{{/if}}
			</div>
		</div>

		<ember:TextAreaField label="Description" field="description" />
		
		<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
	</form>
</div>