/**
 * 
 */
package edu.ucdenver.ccp.sparql_ext;

import java.util.Arrays;

import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.DecimalLiteralImpl;
import org.openrdf.model.impl.IntegerLiteralImpl;
import org.openrdf.model.impl.NumericLiteralImpl;
import org.openrdf.query.algebra.evaluation.ValueExprEvaluationException;
import org.openrdf.query.algebra.evaluation.function.Function;
import org.openrdf.sail.memory.model.DecimalMemLiteral;
import org.openrdf.sail.memory.model.IntegerMemLiteral;
import org.openrdf.sail.memory.model.NumericMemLiteral;

/**
 * A custom SPARQL function that implements the natural logarithm.
 *
 */
public class NaturalLogFunc implements Function {

	/**
	 * The namespace for this custom SPARQL function
	 */
	public static final String NAMESPACE = "http://ccp.ucdenver.edu/sparql/ext/";

	/**
	 * @return the URI "http://ccp.ucdenver.edu/sparql/ext/ln"
	 */
	public String getURI() {
		return NAMESPACE + "ln";
	}

	/**
	 * @return the natural log
	 */
	public Value evaluate(ValueFactory valueFactory, Value... args)
			throws ValueExprEvaluationException {
		if (args.length != 1) {
			throw new ValueExprEvaluationException(
					"The natural log function requires a single argument. "
							+ "An attempt has been made to provide more than one argument: "
							+ Arrays.toString(args));
		}
		Value num = args[0];
		/* throw an error if the single argument is not a number */
		if (!(num instanceof NumericLiteralImpl
				|| num instanceof DecimalLiteralImpl
				|| num instanceof IntegerLiteralImpl
				|| num instanceof NumericMemLiteral
				|| num instanceof IntegerMemLiteral || num instanceof DecimalMemLiteral)) {
			String errorMessage = "The argument to the natural log function must be a number. Observed: "
					+ num;
			System.err.println(errorMessage);
			throw new ValueExprEvaluationException(errorMessage);
		}

		Double d = null;
		if (num instanceof NumericLiteralImpl) {
			d = ((NumericLiteralImpl) num).doubleValue();
		} else if (num instanceof DecimalLiteralImpl) {
			d = ((DecimalLiteralImpl) num).doubleValue();
		} else if (num instanceof IntegerLiteralImpl) {
			d = ((IntegerLiteralImpl) num).doubleValue();
		} else if (num instanceof NumericMemLiteral) {
			d = ((NumericMemLiteral) num).doubleValue();
		} else if (num instanceof IntegerMemLiteral) {
			d = ((IntegerMemLiteral) num).doubleValue();
		} else if (num instanceof DecimalMemLiteral) {
			d = ((DecimalMemLiteral) num).doubleValue();
		}
		Double returnValue = Math.log(d);
		return valueFactory.createLiteral(returnValue);
	}

}
