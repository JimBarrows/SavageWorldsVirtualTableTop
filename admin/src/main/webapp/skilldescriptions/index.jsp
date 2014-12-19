<div class="page-header">
<h1>Skill Descriptions</h1>
</div>
{{#link-to "skilldescriptions.create" class="btn btn-default"}}Add{{/link-to}}
<ul>
	{{#each skill in model}}
	<li>
		{{#link-to 'skilldescriptions.edit' skill}}{{skill.name}}{{/link-to}}  
	</li> 
	{{else}}
	<li>You have no skills!</li>
	{{/each}}
</ul>