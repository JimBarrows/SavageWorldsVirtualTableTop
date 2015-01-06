<form class="form-horizontal" role="form">

	<ember:TextField label="Name" field="name" placeHolder="Name" />

	<div {{bind-attr class=":form-group model.errors.attribute:has-error"}}>
		<label for="era" class="col-sm-2 control-label">Edge Type</label>
		<div class="col-sm-10">
			{{view "select" 
				prompt="Pick an edge type:"
				content=edgeTypeList 
				optionValuePath="content.id"
				optionLabelPath="content.name" 
				selectionBinding="model.edgeType.content"
				class="form-control" }} 
			{{#if model.errors.edgeType}} 
				<span class="glyphicon glyphicon-remove form-control-feedback"></span> 
				<span class="help-block">
					<ul>
						<li>{{#each error in model.errors.edgeType}} {{error.message}} {{/each}}</li>
					</ul>
				</span> 
			{{/if}}
		</div>
	</div>
	
	<div {{bind-attr class=":form-group model.errors.attribute:has-error"}}>
		<label for="era" class="col-sm-2 control-label">Minimum Rank</label>
		<div class="col-sm-10">
			{{view "select" 				
				content=minimumRankList 
				optionValuePath="content.id"
				optionLabelPath="content.name" 
				selectionBinding="model.minimumRank.content"
				class="form-control" }} 
			{{#if model.errors.minimumRank}} 
				<span class="glyphicon glyphicon-remove form-control-feedback"></span> 
				<span class="help-block">
					<ul>
						<li>{{#each error in model.errors.minimumRank}} {{error.message}} {{/each}}</li>
					</ul>
				</span> 
			{{/if}}
		</div>
	</div>
	
	<div {{bind-attr class=":form-group model.errors.attribute:has-error"}}>
		<label for="era" class="col-sm-2 control-label">Required Character Type</label>
		<div class="col-sm-10">
			{{view "select" 				
				content=requiredTypeList 
				optionValuePath="content.id"
				optionLabelPath="content.name" 
				selectionBinding="model.requiredType.content"
				class="form-control" }} 
			{{#if model.errors.requiredType}} 
				<span class="glyphicon glyphicon-remove form-control-feedback"></span> 
				<span class="help-block">
					<ul>
						<li>{{#each error in model.errors.requiredType}} {{error.message}} {{/each}}</li>
					</ul>
				</span> 
			{{/if}}
		</div>
	</div>

	<button {{action 'save'}} class="btn btn-sm btn-primary" role="button">Save</button>
</form>