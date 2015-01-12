package org.savageworlds.admin.rest;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import org.savageworlds.admin.dto.DiceTypeDto;
import org.savageworlds.admin.dto.DiceTypeList;
import org.savageworlds.game.model.DiceType;

@RequestScoped
@Path("/diceTypes")
public class DiceTypeEndpoint {

	@GET
	@Produces("application/json")
	public DiceTypeList listAll() {
		DiceTypeList list = new DiceTypeList();
		for (DiceType val : DiceType.values()) {
			list.add(new DiceTypeDto(val.name(), val.name(), val.ordinal()));
		}
		return list;
	}

	@GET
	@Path("/{id}")
	@Produces("application/json")
	public DiceTypeDto findById(@PathParam("id") final String id) {
		DiceType characterType = DiceType.valueOf(id);
		return new DiceTypeDto(characterType.name(), characterType.name(), characterType.ordinal());
	}

}
