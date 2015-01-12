<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://javadomainobjects.org/tags/ember" prefix="ember"%>
<form class="form-horizontal" role="form">

	<ember:TextField label="Name" field="name" placeHolder="Name" />

	<ember:SelectObjectField label="Edge Type" field="edgeType"
		list="edgeTypeList" />

	<ember:SelectObjectField label="Minimum Rank" field="minimumRank"
		list="minimumRankList" />

	<ember:SelectObjectField label="Required Character Type"
		field="requiredType" list="requiredTypeList" />

	<div {{bind-attr class=":form-group model.errors.minimumSkills:has-error"}}>
	<label for="era" class="col-sm-2 control-label">Minimum Skills</label>
	<div class="col-sm-10">		
	<table class="table table-striped">
		<thead>
			<tr>
				<td>Description</td>
				<td>Rank</td>
			</tr>
		</thead>
		<tbody>
			<tr>
				<td>{{view "select" 
						content=skillDescriptionList 
						optionValuePath="content.id"
						optionLabelPath="content.name" 
						selectionBinding="skillDescription"
						class="form-control" }}
				</td>
				<td>{{view "select" 
						content=diceTypeList 
						optionValuePath="content.id"
						optionLabelPath="content.name" 
						selectionBinding="diceType"
						class="form-control" }}</td>
				<td><button {{action 'addSkillDescription'}} class="btn btn-xs btn-success" role="button"><span class="glyphicon glyphicon-plus" aria-hidden="true"></span></button></td>
			</tr>
			{{#each skill in model.minimumSkills}}
				<tr>
					<td>{{skill.description.name}}</td>
					<td>{{skill.dice.name}}</td>
					<td>
						<button class="btn btn-xs btn-success" role="button"><span class="glyphicon glyphicon-plus" aria-hidden="true"></span></button>
					</td>
				</tr>
			{{else}}
				<tr><td colspan="3">There is no minimum skills for this edge</td></tr>
			{{/each}}
		</tbody>
	</table>
	</div>

	<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
</form>