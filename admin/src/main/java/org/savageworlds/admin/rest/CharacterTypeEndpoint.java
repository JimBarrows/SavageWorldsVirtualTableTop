package org.savageworlds.admin.rest;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import org.savageworlds.admin.dto.CharacterTypeDto;
import org.savageworlds.admin.dto.CharacterTypeList;
import org.savageworlds.game.model.CharacterType;

@RequestScoped
@Path("/characterTypes")
public class CharacterTypeEndpoint {

	@GET
	@Produces("application/json")
	public CharacterTypeList listAll() {
		CharacterTypeList list = new CharacterTypeList();
		for( CharacterType val : CharacterType.values()) {
			list.add( new CharacterTypeDto( val.name(), val.name()));
		}
		return list;
	}
	
	@GET
	@Path("/{id}")
	@Produces("application/json")
	public CharacterTypeDto findById(@PathParam("id") final String id) {
		CharacterType characterType = CharacterType.valueOf(id);
		return new CharacterTypeDto( characterType.name(), characterType.name());
	}

}
