<div class="page-header">
<h1>Edge Types</h1>
</div>
{{#link-to "edgetypes.create" class="btn btn-default"}}Add{{/link-to}}
<dl>
	{{#each record in model}}
	<dt>
		{{#link-to 'edgetypes.edit' record}}{{record.name}}{{/link-to}}
		<button type="button" class="btn btn-default btn-xs" aria-label="Left Align" {{action "remove" record}}>
  			<span class="glyphicon glyphicon-remove" aria-hidden="true"></span>
		</button> 
	</dt>
	<dd>{{{record.description}}}</dd> 
	{{else}}
	<li>You have no edge types!</li>
	{{/each}}
</ul>