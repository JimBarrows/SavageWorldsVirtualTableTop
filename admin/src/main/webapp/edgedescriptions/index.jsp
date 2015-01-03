<div class="page-header">
<h1>Edge Description</h1>
</div>
{{#link-to "edgedescriptions.create" class="btn btn-default"}}Add{{/link-to}}
<ul>
	{{#each record in model}}
	<li>
		{{#link-to 'edgedescriptions.edit' record}}{{record.name}}{{/link-to}}
		<button type="button" class="btn btn-default" aria-label="Left Align" {{action "remove" record}}>
  			<span class="glyphicon glyphicon-remove" aria-hidden="true"></span>
		</button> 
	</li> 
	{{else}}
	<li>You have no edge descriptions!</li>
	{{/each}}
</ul>