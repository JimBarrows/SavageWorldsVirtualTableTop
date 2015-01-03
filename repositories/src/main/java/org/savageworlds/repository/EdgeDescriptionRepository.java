package org.savageworlds.repository;

import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;

import jdo.core.repository.RepositoryTemplate;

import org.savageworlds.game.model.EdgeDescription;

@Stateful()
public class EdgeDescriptionRepository extends RepositoryTemplate<EdgeDescription, Long> {

	public EdgeDescriptionRepository() {
		super(EdgeDescription.class);
	}

	@PersistenceContext(name = "SavageWorlds", type=PersistenceContextType.EXTENDED)
	protected EntityManager	em;

	@Override
	protected EntityManager em() {
		return em;
	}
}
