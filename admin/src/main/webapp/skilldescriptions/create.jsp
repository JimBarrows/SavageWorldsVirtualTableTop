<div class="container">
	<div class="page-header">
		<h1>Create Skill</h1>
	</div>
	<form class="form-horizontal" role="form">
		<div {{bind-attr class=":form-group model.errors.name:has-error"}}>
			<label for="name" class="col-sm-2 control-label">Name</label>
			<div class="col-sm-10">
				{{input type="text" value=model.name class="form-control" placeholder="Skill Name"}} 
				{{#if model.errors.name}} 
					<span class="glyphicon glyphicon-remove form-control-feedback"></span> 
					<span class="help-block">
						<ul>
							<li>{{#each model.errors.name}} {{message}} {{/each}}</li>
						</ul>
					</span> 
				{{/if}}
			</div>
		</div>

		<div {{bind-attr class=":form-group model.errors.attribute:has-error"}}>
			<label for="attribute" class="col-sm-2 control-label">Attribute</label>
			<div class="col-sm-10">
				{{input type="text" value=model.attribute class="form-control" placeholder="Attribute"}} 
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

		<div {{bind-attr class=":form-group model.errors.description:has-error"}}>
			<label for="description" class="col-sm-2 control-label">Description</label>
			<div class="col-sm-10">
				{{textarea value=model.description class="form-control" placeholder="Description of Skill"}} 
				{{#if model.errors.description}} 
					<span class="glyphicon glyphicon-remove form-control-feedback"></span> 
					<span class="help-block">
						<ul>
							<li>{{#each model.errors.description}} {{message}} {{/each}}</li>
						</ul>
					</span> 
				{{/if}}
			</div>
		</div>
		<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
	</form>
</div>