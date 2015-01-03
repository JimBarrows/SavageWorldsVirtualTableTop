package org.savageworlds.repository;

import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;

import jdo.core.repository.RepositoryTemplate;

import org.savageworlds.game.model.EdgeType;

@Stateful
public class EdgeTypeRepository extends RepositoryTemplate<EdgeType, Long> {

	public EdgeTypeRepository() {
		super(EdgeType.class);
	}

	@PersistenceContext(name = "SavageWorlds", type = PersistenceContextType.EXTENDED)
	protected EntityManager	em;

	@Override
	protected EntityManager em() {
		return em;
	}

}
