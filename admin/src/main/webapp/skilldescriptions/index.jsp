<div class="page-header">
<h1>Skill Descriptions</h1>
</div>
{{#link-to "skilldescriptions.create" class="btn btn-default"}}Add{{/link-to}}
<ul>
	{{#each skill in model}}
	<li>
		{{#link-to 'skilldescriptions.edit' skill}}{{skill.name}}({{skill.attribute}}){{/link-to}}
		<button type="button" class="btn btn-default" aria-label="Left Align" {{action "remove" skill}}>
  			<span class="glyphicon glyphicon-remove" aria-hidden="true"></span>
		</button> 
	</li> 
	{{else}}
	<li>You have no skills!</li>
	{{/each}}
</ul>