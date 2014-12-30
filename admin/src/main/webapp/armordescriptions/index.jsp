<div class="page-header">
<h1>Armor Descriptions</h1>
</div>
{{#link-to "armordescriptions.create" class="btn btn-default"}}Add{{/link-to}}
<ul>
	{{#each armor in model}}
	<li>
		{{#link-to 'armordescriptions.edit' armor}}{{armor.name}}{{/link-to}}
		<button type="button" class="btn btn-default" aria-label="Left Align" {{action "remove" armor}}>
  			<span class="glyphicon glyphicon-remove" aria-hidden="true"></span>
		</button> 
	</li> 
	{{else}}
	<li>You have no armor!</li>
	{{/each}}
</ul>