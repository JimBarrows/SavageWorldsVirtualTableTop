<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://javadomainobjects.org/tags/ember" prefix="ember"%>
<div class="container">
	<div class="page-header">
		<h1>Edit Armor Description</h1>
	</div>
	<form class="form-horizontal" role="form">

		<ember:TextField label="Name" field="name" placeHolder="Name" />

		<ember:NumericField label="Armour" field="armor" />

		<ember:NumericField label="vs Bullets" field="vsBullets" />

		<ember:NumericField label="Weight" field="weight" />

		<ember:NumericField label="Cost" field="cost" />

		<div {{bind-attr class=":form-group model.errors.attribute:has-error"}}>
			<label for="era" class="col-sm-2 control-label">Era</label>
			<div class="col-sm-10">
				{{view "select" content=eras selection=model.era
				class="form-control" prompt="Please select an era"}} {{#if
				model.errors.era}} <span
					class="glyphicon glyphicon-remove form-control-feedback"></span> <span
					class="help-block">
					<ul>
						<li>{{#each model.errors.era}} {{message}} {{/each}}</li>
					</ul>
				</span> {{/if}}
			</div>
		</div>

		<ember:TextAreaField label="Notes" field="notes" />


		<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
	</form>
</div>