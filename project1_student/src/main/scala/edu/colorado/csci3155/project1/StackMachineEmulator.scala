package edu.colorado.csci3155.project1

import scala.annotation.tailrec
import scala.math.exp



sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case PushNumI(d: Double) => 
            {
                val newd:Value = Num(d)
                (newd::stack, env)
            }
            case PushBoolI(b) => {
                val newB:Value  = Bool(b)
                    (newB::stack, env)
            }
            case AddI => 
            {
                if(stack.isEmpty)
                    throw new RuntimeException ("Empty Stack")
                else
                {
                    val h1:Value = stack.head;
                    val h2:Value  = stack.tail.head;
                  
                   (h1,h2) match
                    {
                        case (Num(n1), Num(n2)) =>
                        { 
                            val result: Double = n1 + n2
                            val newResult: Value = Num(result); //Converting result into a Val via the Num class constructor
                            (newResult::stack.tail.tail, env)
                        }
                        case (_, _) =>  throw new RuntimeException ("Type Mismatch")
                    }
                    
                }
            }
            case SubI =>
                {

                    if(stack.isEmpty)
                        throw new RuntimeException ("Empty Stack")
                    else
                    {
                        val h1:Value = stack.head;
                        val h2:Value  = stack.tail.head;
                    
                    (h1,h2) match
                        {
                            case (Num(n1), Num(n2)) =>
                            { 
                                val result: Double = n2 - n1
                                val newResult: Value = Num(result); //Converting result into a Val via the Num class constructor
                                (newResult::stack.tail.tail, env)
                            }
                            case (_, _) =>  throw new RuntimeException ("Type mismatch")
                        }
                    }
                }
            case MultI =>
                {
                    if(stack.isEmpty)
                        throw new RuntimeException ("Empty Stack")
                    else
                    {
                        val h1:Value = stack.head;
                        val h2:Value  = stack.tail.head;
                    
                    (h1,h2) match
                        {
                            case (Num(n1), Num(n2)) =>
                            { 
                                val result: Double = n1 * n2
                                val newResult: Value = Num(result)
                                (newResult::stack.tail.tail, env)
                            }
                            case (_, _) =>  throw new RuntimeException ("Type Mismatch")
                        }
                    }
                }
            case DivI =>
                {
                    if(stack.isEmpty)
                        throw new RuntimeException ("Empty Stack")
                    else
                    {
                        val h1:Value = stack.head;
                        val h2:Value  = stack.tail.head;
                    
                        (h1,h2) match
                        {
                            case (Num(n1), Num(n2)) if n1 != 0 =>
                                { 
                                    val result: Double = n2/ n1
                                    val newResult: Value = Num(result)
                                    (newResult::stack.tail.tail, env)
                                }
                            case (_, _) =>  throw new RuntimeException ("Type mismatch")
                        }
                    }
                }
                case ExpI =>
                {
                    if(stack.isEmpty)
                        throw new RuntimeException ("Empty Stack")
                    val h1:Value = stack.head;
                    (h1) match
                    {
                        case(Num(n1)) => 
                        {
                            val result: Double = exp(n1)
                            val newResult: Value = Num(result); //Converting result into a Val via the Num class constructor
                            (newResult::stack.tail, env)
                        }
                        case (_) => throw new RuntimeException ("Type Mismatch")
                    }
                }
                case LogI => 
                {
                    if(stack.isEmpty)
                        throw new RuntimeException("Empty Stack")
                    else
                        {
                            val h1: Value = stack.head;
                            (h1) match
                            {
                                case (Num(n1)) if n1 >= 0 =>
                                {
                                    val result: Double = math.log(n1)
                                    val newResult: Value = Num(result)
                                    (newResult::stack.tail, env)
                                }
                                case (_) =>  throw new RuntimeException ("Type mismatch")
                            }
                        }
                    }
                    case  SinI  =>
                    {
                        if(stack.isEmpty)
                            throw new RuntimeException ("Empty Stack")
                        else
                        {
                                val h1: Value = stack.head;
                                (h1) match 
                                {
                                    case (Num(n1)) =>
                                    {
                                        val result: Double = math.sin(n1)
                                        val newResult: Value = Num(result); //Converting result into a Val via the Num class constructor
                                        (newResult::stack.tail, env)
                                    }
                                    case (_) => throw new RuntimeException ("Type Mismatch")
                                }
                        }
                    }
                    case  CosI  =>
                        {
                            if(stack.isEmpty)
                                throw new RuntimeException ("Empty Stack")
                            else
                                {
                                    val h1: Value = stack.head;
                                    (h1) match 
                                    {
                                        case (Num(n1)) =>
                                        {
                                            val result: Double = math.cos(n1)
                                            val newResult: Value = Num(result); //Converting result into a Val via the Num class constructor
                                            (newResult::stack.tail, env)
                                        }
                                        case (_) => throw new RuntimeException ("Type Mismatch")
                                    }
                                }
                        }
                        case GeqI => 
                        {
                            if(stack.isEmpty)
                                    throw new RuntimeException ("Empty Stack")
                            else
                            {
                                    val h1:Value = stack.head;
                                    val h2:Value  = stack.tail.head;
                                    (h1,h2) match
                                    {
                                        case (Num(n1), Num(n2)) => 
                                        {
                                            val result: Value = Bool(n2 >= n1)
                                            (result::stack.tail, env)
                                        }
                                        case (_, _) =>  throw new RuntimeException ("Type mismatch")
                                    }
                            }
                        }
                        case PopI => 
                        {
                            if(stack.isEmpty)
                                throw new RuntimeException ("Stack Empty")
                            else
                            {
                                val top = stack.head //Takes the top value of the stack
                                val bottom  = stack.tail //Take the next value
                                (bottom, env) //Pass in bottom value and env
                            }
                        }
                        case EqI =>
                        {
                            if(stack.isEmpty)
                                throw new RuntimeException ("Stack Empty")
                            else
                            {
                                val h1: Value = stack.head;
                                val h2: Value = stack.tail.head;
                                (h1,h2) match
                                {
                                    case(Num(n1), Num(n2)) => 
                                    {
                                        val result: Value = Bool(n1 == n2)
                                        (result::stack, env)
                                    }
                                    case (Bool(n1), Bool(n2)) => 
                                    {
                                        val result: Value = Bool(n1 == n2)
                                        (result::stack, env)
                                    }
                                    case (_ , _) =>  throw new RuntimeException ("Type mismatch")
                                }
                            }
                        }
                        case NotI =>
                        {
                            if(stack.isEmpty)
                                throw new RuntimeException ("Stack Empty")
                            else
                            {
                                val h1:Value = stack.head;
                                (h1) match
                                    {
                                        case (Bool(n1)) => 
                                        {
                                            val result: Value = Bool(!n1);
                                            (result::stack, env)
                                        }
                                        case (_) => throw new RuntimeException ("Type mismatch")
                                    }
                            }
                        }
                        case LoadEnv(identifier) => 
                        {
                            if(stack.isEmpty)
                                throw new RuntimeException ("Empty Stack")
                            else
                                {
                                    val v:Value = stack.head;
                                    (stack.tail, (identifier, v) :: env)
                                    
                                }
                        }
                        case StoreEnv(identifier) => 
                        {
                            if(env.isEmpty)
                                throw new RuntimeException ("Empty Stack")
                            else
                            {
                                env.find{case(id, _) => id == identifier } match
                                {
                                    case Some((_, t2)) =>
                                    {
                                        (t2:: stack, env)
                                    }
                                    case None =>  throw new RuntimeException (s"The value $identifier is not found")
                                }
                            }
                        }

                     case PopEnv =>
                    {
                        if(stack.isEmpty)
                            throw new RuntimeException ("Empty Stack")
                        else
                        {
                            (stack, env.tail)
                        }
                    }
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}