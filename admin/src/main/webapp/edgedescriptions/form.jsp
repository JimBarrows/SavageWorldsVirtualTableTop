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

	<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
</form>